;;; magit-gh-comments-core.el --- Comment on Github Pull Requests via Magit  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2018 Andrew Stahlman

;; Author: Andrew Stahlman <andrewstahlman@gmail.com>
;; Created: 28 Jun 2018
;; Version: 0.1

;; Keywords: tools git vc
;; Homepage: https://github.com/astahlman/magit-gh-comments

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Package-Requires: ((magit "2.11.0") (magit-gh-pulls "0.5.3") (request "0.3.0") (emacs "26.0"))

;;; Commentary:
;; This package enables the user to view and post comments on Github
;; Pull Requests.

;;; Code:

;; (add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

(require 'cl)
(require 'dash)
(require 'ert)
(require 'ht)
(require 'magit-gh-comments-github)
(require 'magit-gh-comments-diff)
(require 'magit-gh-comments-prose)
(require 'magit-gh-comments-utils)

;; - A-OR-B is the revision we're looking at, :a or :b (old or new)
;; - HUNK-START corresponds to the number in the hunk-header
;; - OFFSET is the # of lines *below* the hunk header, i.e., the first line in a hunk is at offset=1
(defstruct magit-gh-diff-pos a-or-b hunk-start offset)

;; - ID is the comment identifier from Github
;; - REVIEW-ID is the review identifier from Github
;; - FILE is the name of the file to which the comment applies
;; - COMMIT-SHA is the SHA of the git commit to which the comment applies
;; - TEXT is the body of the comment
;; - AUTHOR is the Github handle of the commenter
;; - CREATED_AT is the ISO-8601 timestamp at which the comment was created
;; - GH-POS is the Github position of the comment (an integer)
;; - ORIGINAL-GH-POS is only set if this comment is outdated
;; - IS-OUTDATED is t iff the comment does not apply to the most recent commit
;; - IN-REPLY-TO if this comment belongs to a thread, this is the ID of the original comment
(defstruct magit-gh-comment
  id
  review-id
  file
  commit-sha
  text
  author
  created-at
  gh-pos
  original-gh-pos
  is-outdated
  in-reply-to)

;; [Ugly hack]: redefine the pull-section keymap from magit-gh-pulls.el
;; Instead of jumping to a magit-diff, we'll open a buffer that shows all
;; of the reviews and comments

;;;###autoload
(setq magit-pull-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing]      'magit-gh-show-pr)
    map))

(defun magit-pull-request--buffer-name (mode lock-value)
  (-let [(owner repo-name number) lock-value]
    (format "PR: %s (#%s)"
            repo-name
            number)))

(defun magit-review--buffer-name (mode lock-value)
  (-let [(owner repo-name number) lock-value]
    (format "PR Review: %s (#%s)"
            repo-name
            number)))

;;;###autoload
(defun magit-gh-reload-from-github ()
  (interactive)
  (let ((magit-gh--should-skip-cache t))
    (magit-refresh-buffer)))

(defun magit-gh-show-pr (&optional pr)
  (interactive)
  (let ((pr (or pr (magit-gh--capture-current-pull-request)))
        (magit-generate-buffer-name-function #'magit-pull-request--buffer-name))
    (magit-mode-setup-internal 'magit-pull-request-mode (list pr) t)))

(defun magit-gh--sort (seq key-fn &optional pred)
  "Return a copy of SEQ, stably-sorted based on a given key.

KEY-FN is applied to each element of SEQ to obtain a value used
as the basis for ordering. PRED is the function used for
comparison and defaults to #'<. SEQ is not modified.

Example usage:

\(magit-gh--sort '(-100 1 2) #'abs) -> '(1 2 -100)
"
  (let ((pred (or pred #'<)))
    (sort (copy-list seq) (lambda (x y)
                            (funcall pred
                                     (funcall key-fn x)
                                     (funcall key-fn y))))))

(ert-deftest test-magit-gh--sort ()
  (should (equal (magit-gh--sort '("z" "a" "f") #'identity #'string<)
                 '("a" "f" "z")))
  (should (equal (magit-gh--sort '(-100 1 2) #'abs)
                 '(1 2 -100))))

(defmacro letfn (fn-defs &rest body)
  (declare (indent 1) (debug ((&rest (sexp function-form)) body)))
  (let ((bindings (mapcar
                   (lambda (fn-def) `((symbol-function (quote ,(car fn-def))) ,(cadr fn-def)))
                   fn-defs)))
    `(cl-letf ,bindings
       ,@body)))


(defmacro magit-gh--with-temp-buffer (&rest forms)
  "Like `with-temp-buffer', but keep the buffer around for
debugging during testing."
  (declare (indent 0) (debug t))
  `(if (ert-running-test)
       (progn
         (when (get-buffer "magit-gh-temp-buffer")
           (kill-buffer "magit-gh-temp-buffer"))
         (with-current-buffer (get-buffer-create "magit-gh-temp-buffer")
           ,@forms))
     (with-temp-buffer ,@forms)))

(defun magit-gh--cur-magit-diff-pos ()
  "Return the current, magit-style position in the diff."
  (interactive)
  (let* ((hunk-header (magit-gh--try-parse-hunk-header2)) ;; TODO: How will this handle new files?
         (rev (save-excursion (goto-char (line-beginning-position))
                              (if (looking-at "^-") :a :b)))
         (hunk-start (alist-get (if (eq :a rev) :a-start :b-start) hunk-header))
         (target-line (line-number-at-pos))
         (offset 0))
    (save-excursion
      (magit-section-backward)
      (while (< (line-number-at-pos) target-line)
        (unless (looking-at
                 (format "^%s" (if (eq rev :a) "\\+" "-")))
          (cl-incf offset))
        (assert (= 0 (forward-line))))
      (make-magit-gh-diff-pos :a-or-b (if (eq :a rev) :a :b)
                              :hunk-start hunk-start
                              :offset offset))))

(defun magit-gh--cur-github-diff-pos ()
  "Return the current, Github-style position in the diff."
  (interactive)
  (let* ((magit-pos (magit-gh--cur-magit-diff-pos)) ;; TODO: What if the line at point isn't part of a hunk/isn't changed by this diff?
         (diff-body (magit-gh--fetch-diff-from-github (magit-gh--get-current-pr))))
    (magit-gh--diff-pos/magit->gh (magit-current-file)
                                  magit-pos
                                  diff-body)))

(defun magit-gh--filter-sections (pred &optional parent)
  "Return every child section of PARENT that matches PRED.

PARENT defaults to `magit-root-section'"
  (-filter
   pred
   (oref (or parent magit-root-section) children)))

(defun magit-gh--visit-diff-pos (file pos)
  "Put point at the given position POS in a magit-diff buffer.

Go to the beginning of the line corresponding to
magit-gh-diff-pos POS in the section for FILE."
  (-if-let* ((rev (magit-gh-diff-pos-a-or-b pos))
             (target (+ (magit-gh-diff-pos-offset pos)
                        (magit-gh-diff-pos-hunk-start pos)))
             (file-section (car (magit-gh--filter-sections
                                 (lambda (sec) (and (magit-file-section-p sec)
                                                    (string= (oref sec value)
                                                             file))))))
             (target-in-hunk-p (lambda (hunk-header)
                                 (let ((start (alist-get (if (eq :a rev) :a-start :b-start)
                                                         hunk-header))
                                       (len (alist-get (if (eq :a rev) :a-len :b-len)
                                                       hunk-header)))
                                   (and (>= target start)
                                        (<= target (+ start len))))))
             (hunk-section (magit-gh--filter-sections
                            (lambda (sec)
                              (if-let ((hunk-header (magit-gh--try-parse-hunk-header2 sec)))
                                  (funcall target-in-hunk-p hunk-header)))
                            file-section)))
      (progn
        (setq file-sec file-section)
        (assert (= 1 (length hunk-section)))
        (magit-section-goto (car hunk-section))
        (let* ((found-p nil)
               (i 0)
               (hunk-header (magit-gh--try-parse-hunk-header2))
               (start (alist-get (if (eq :a rev) :a-start :b-start)
                                 hunk-header)))
          (while (< (+ start i) target)
            (assert (= 0 (forward-line)))
            (unless (looking-at
                     (format "^%s" (if (eq :a rev) "\\+" "-")))
              (cl-incf i)))))
    ;; TODO: Decide how to handle this. Reducing from `error' to
    ;; `warn' for now because it keeps blocking my normal git workflow
    ;; during `magit-refresh'
    (warn "Could not find a hunk that contains magit-position [%s]" pos)))

(defun magit-gh--try-parse-hunk-header (&optional line)
  "Try to parse the current LINE as a hunk-header.

Return nil if the line at point is not a hunk-header."
  (interactive)
  (let ((line (or line (buffer-substring (point-at-bol) (point-at-eol)))))
    (with-temp-buffer
      (insert line)
      (goto-char (point-min))
      (if (looking-at magit-gh--hunk-header-re)
          `((:a-start . ,(string-to-number (match-string 1)))
            (:a-len . ,(string-to-number (match-string 2)))
            (:b-start . ,(string-to-number (match-string 3)))
            (:b-len . ,(string-to-number (match-string 4))))))))

;; TODO: Combine this and the above method
(defun magit-gh--try-parse-hunk-header2 (&optional section)
  (let ((section (or section (magit-current-section))))
    (if (magit-section-match 'hunk section)
        (cl-destructuring-bind (a b) (cdr (oref section value))
          (let ((a-range (mapcar #'string-to-number (split-string a ",")))
                (b-range (mapcar #'string-to-number (split-string b ","))))
            `((:a-start . ,(* -1 (car a-range))) ;; string starts with a '-'
              (:a-len . ,(cadr a-range))
              (:b-start . ,(car b-range))
              (:b-len . ,(cadr b-range))))))))


(defun magit-gh--partition-by (xs-in fn &optional xs-out)
  "Partition XS-IN according to the function FN.

Apply FN to each value in XS-IN, splitting it each time FN
returns a new value.  Return a list of lists."
  (let ((max-lisp-eval-depth (max max-lisp-eval-depth 99999))
        (max-specpdl-size (max max-specpdl-size 99999)))
    (if (not xs-in)
        (reverse (cons (reverse (car xs-out)) (cdr xs-out)))
      (cond ((not xs-out)
             (magit-gh--partition-by (cdr xs-in)
                                     fn
                                     (cons (list (car xs-in)) '())))
            ((equal (funcall fn (car xs-in))
                    (funcall fn (caar xs-out)))
             (magit-gh--partition-by (cdr xs-in)
                                     fn
                                     (cons (cons (car xs-in) (car xs-out)) (cdr xs-out))))
            (:t
             (magit-gh--partition-by (cdr xs-in)
                                     fn
                                     (cons (list (car xs-in)) (cons (reverse (car xs-out)) (cdr xs-out)))))))))

(ert-deftest test-magit-gh--partition-by ()
  (should (equal '((0 1) (2 3) (4 5))
                 (magit-gh--partition-by '(0 1 2 3 4 5) (lambda (n) (/ n 2)))))
  (should (equal '((:a :b :c) (:d))
                 (magit-gh--partition-by '(:a :b :c :d)  (lambda (x) (eq :d x))))))

(defun magit-gh--pair-and-merge (xs-in &optional xs-out)
  "Pair and merge adjacent sequences in XS-IN.

Return a sequence of sequences which is half the length
of the original.

Example:

'((:foo) (:bar) (:buzz) (:bop)) -> '((:foo :bar) (:buzz :bop))"
  (assert (evenp (length xs-in)))
  (if (not xs-in)
      (reverse xs-out)
    (magit-gh--pair-and-merge (cddr xs-in) (cons (append (car xs-in) (cadr xs-in)) xs-out))))

(ert-deftest test-magit-gh--pair-and-merge ()
  (should (equal '((1 2 3 4) (5 6 7 8))
                 (magit-gh--pair-and-merge '((1) (2 3 4) (5 6) (7 8))))))

(defun magit-gh--running-sum (xs)
  "Calculate a running sum of the elements of XS.

Return a sequence of the same length as XS, where the element
at each index i in the output equals the sum of XS[0..i]

Example:

'(1 2 0) -> '(1 3 3)"
  (reverse
   (let (result)
     (dolist (x xs result)
       (setq result (cons (+ (or (car result) 0) x)
                          result))))))

(ert-deftest test-magit-gh--running-sum ()
  (should (equal '(1 2 2 4 4 4)
                 (magit-gh--running-sum '(1 1 0 2 0 0))))
  (should (= 5050
             (car (last (magit-gh--running-sum (number-sequence 1 100)))))))

(defun magit-gh--calc-gh-offset (lines pos)
   "Given hunk body LINES and magit diff position POS, calculate
the (Github-style) position within the hunk as 1 + the number of
lines in the hunk that precede POS."
  (let* ((magit-offset (magit-gh-diff-pos-offset pos))
         (char-to-ignore (if (eq (magit-gh-diff-pos-a-or-b pos) :a) ?+ ?-))
         (lines-preceding-target (seq-take-while (lambda (n) (< n magit-offset))
                                                (magit-gh--running-sum
                                                 (mapcar (lambda (line)
                                                           (if (= char-to-ignore (elt line 0)) 0 1))
                                                         lines)))))
    (if (= (length lines-preceding-target)
             (length lines))
        (error "Error! Position not found")
      (1+ (length lines-preceding-target)))))

(ert-deftest test-magit-gh--calc-gh-offset ()
  (let ((make-pos (lambda (a-or-b offset)
                    (make-magit-gh-diff-pos :a-or-b a-or-b
                                            :offset offset)))
        (lines '("-foo" "+bar" "buzz" "-bam")))
    (should (= 1
               (magit-gh--calc-gh-offset lines (funcall make-pos :a 1))))
    (should (= 2
               (magit-gh--calc-gh-offset lines (funcall make-pos :b 1))))
    (should (= 3
               (magit-gh--calc-gh-offset lines (funcall make-pos :b 2))))
    (should (= 3
               (magit-gh--calc-gh-offset lines (funcall make-pos :a 2))))
    (should (= 4
               (magit-gh--calc-gh-offset lines (funcall make-pos :a 3))))
    (should-error (magit-gh--calc-gh-offset lines (funcall make-pos :b 3)))
    (should-error (magit-gh--calc-gh-offset lines (funcall make-pos :a 4)))))

(defun magit-gh--try-parse-file-header (line)
  "Try to parse the given LINE as a file header.

   Given a LINE from the body of a diff (of type
application/vnd.github.v3.diff, from Github), try to parse it as
the header of a new section in the diff.

   Example:

   (magit-gh--try-parse-file-header
       \"diff --git a/magit-gh-comments-github.el b/magit-gh-comments-github.el\")


   -> '((:a . \"magit-gh-comments-github.el\")
        (:b . \"magit-gh-comments-github.el\"))"

  (let ((file-header-re "^diff --git a/\\([^ ]+\\) b/\\([^ ]+\\)$"))
    (when (string-match file-header-re line)
      `((:a . ,(match-string-no-properties 1 line))
        (:b . ,(match-string-no-properties 2 line))))))

(ert-deftest test-magit--try-parse-file-header ()
  (should (equal '((:a . "foo")
                   (:b . "bar"))
             (magit-gh--try-parse-file-header "diff --git a/foo b/bar")))
  (should-not (magit-gh--try-parse-file-header "+ This is not a match")))


(defun magit-gh--diff-pos/magit->gh (file pos diff-body)
  "Convert a Magit diff position to a Github diff position.

   Given magit diff position POS and a DIFF-BODY (of type
application/vnd.github.v3.diff, from Github), calculate the
corresponding Github-style position."
  (let* ((lines (split-string diff-body "\n" t))
         ;; apparently git strips the leading slash from file paths
         ;; e.g., 'diff --git a/tmp/foo b/tmp/foo', not 'diff --git a//tmp/foo b//tmp/foo'
         (clean-filename (replace-regexp-in-string "^\/" "" file))
         (file-sections (magit-gh--pair-and-merge
                         (magit-gh--partition-by lines
                                                 'magit-gh--try-parse-file-header)))
         (lines-for-file (car (-filter (lambda (file-section)
                                         (string-match
                                          (format "diff --git a/[^ ]+ b/%s$" clean-filename)
                                          (car file-section)))
                                       file-sections)))
         (lines-for-file (seq-drop-while
                          ;; discard everything before the first hunk header
                          (lambda (x) (not (magit-gh--try-parse-hunk-header x)))
                          lines-for-file))
         (lines-partitioned-by-hunk-header
          (magit-gh--partition-by lines-for-file 'magit-gh--try-parse-hunk-header))
         (hunks (magit-gh--pair-and-merge lines-partitioned-by-hunk-header))
         (hunks-not-after-target (seq-take-while
                                  (lambda (hunk)
                                    (let* ((header (magit-gh--try-parse-hunk-header (car hunk)))
                                           (hunk-start (alist-get (if (eq (magit-gh-diff-pos-a-or-b pos) :a)
                                                                      :a-start
                                                                    :b-start)
                                                                  header)))
                                      (<= hunk-start (magit-gh-diff-pos-hunk-start pos))))
                                  hunks))
         (is-valid (if (not hunks-not-after-target)
                       (user-error "Could not find a line at diff position [%s]. Last hunk: [%s]"
                                   pos
                                   (caar (last hunks-not-after-target)))))
         (gh-pos-hunk-start (reduce '+ (mapcar #'length (butlast hunks-not-after-target))))
         (last-hunk-content (cdar (last hunks-not-after-target)))
         (gh-hunk-offset (magit-gh--calc-gh-offset last-hunk-content pos)))
    (+ gh-pos-hunk-start
       gh-hunk-offset)))

(defun magit-gh--diff-pos/gh->magit (file gh-pos gh-diff-body)
  "Convert a Github diff position to a Magit diff position.

Given a FILE and a Github-style diff position GH-POS in
GH-DIFF-BODY, return the corresponding magit-gh-diff-pos."
  (with-temp-buffer
    (save-excursion
      (insert gh-diff-body)
      (goto-char (point-min))
      ;; FIXME: This regex isn't very robust
      (re-search-forward (format "+++ b/%s" file))
      (re-search-forward magit-gh--hunk-header-re)
      (forward-line gh-pos)
      (let ((rev (if (looking-at "^-") :a :b))
            (offset 0))
        (while (not (magit-gh--try-parse-hunk-header))
          (unless (or (and (eq rev :a) (looking-at "^\\+"))
                      (and (eq rev :b) (looking-at "^-")))
            (cl-incf offset))
          (assert (= 0 (forward-line -1))))
        (make-magit-gh-diff-pos :a-or-b rev
                                :hunk-start (alist-get
                                             (if (eq rev :a) :a-start :b-start)
                                             (magit-gh--try-parse-hunk-header))
                                :offset offset ;;(1- offset)
                                )))))

(defun magit-gh--section-content-as-string (&optional section include-heading)
  "Return the content of SECTION as a string.

If SECTION is not supplied, use the value of
`magit-current-section'."
  (let ((section (or section (magit-current-section))))
    (save-excursion
      (with-current-buffer (marker-buffer (oref section end))
        (buffer-substring (if include-heading
                              (oref section start)
                            (or (oref section content)
                                (oref section start)))
                          (oref section end))))))

;;;###autoload
(defun magit-gh-reply-to-comment (id comment-text)
  "Reply to the comment with ID with COMMENT-TEXT"
  (magit-gh--reply-to-comment (magit-gh--get-current-pr) id comment-text)
  (magit-gh-reload-from-github))

;;;###autoload
(defun magit-gh-add-comment (arg comment-text)
  "Comment on the line at point and post COMMENT-TEXT to Github.

If called with a prefix arg, the comment is posted immediately to
Github. Else, the comment is saved in a pending review."
  (interactive "P\nMComment: ")
  (let ((post-immediately arg)
        (current-pr (magit-gh--get-current-pr))
        (commit-sha (cdr (magit-split-range (magit-diff--dwim)))))
    (if post-immediately
        (let ((github-pos (magit-gh--cur-github-diff-pos)))
          (magit-gh--post-pr-comment current-pr
                                     (magit-current-file) ;; FIXME: Add current-file to github-pos, get rid of this arg
                                     commit-sha
                                     github-pos
                                     comment-text))
      ;; else, save this comment for later
      (let ((comment (make-magit-gh-comment :file (magit-current-file)
                                            :commit-sha commit-sha
                                            :gh-pos (magit-gh--cur-github-diff-pos)
                                            :text comment-text))
            (pending-review (or (magit-gh--get-review-draft current-pr)
                                (make-magit-gh-review :commit-sha commit-sha
                                                      :state 'pending))))
        (setf (magit-gh-review-threads pending-review)
              (cons (list comment)
                    (magit-gh-review-threads pending-review)))
        (magit-gh--store-review-draft pending-review current-pr)))))

;; TODO: Move this into our own keymap
(magit-define-popup-action 'magit-gh-pulls-popup
  ?k "Comment on line at point" #'magit-gh-add-comment)

(defun magit-gh--format-comment-body (s &optional fill-width)
  "Format the string S as the body of a Github comment.

Fill the paragraph up to FILL-WIDTH characters."
  (with-temp-buffer
    (insert s)
    (let ((fill-column (or fill-width 80)))
      (fill-region (point-min) (point-max)))
    (goto-char (point-min))
    (while (re-search-forward "^" nil t)
      (replace-match ">>> "))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun magit-gh--propertize-overlay-text (s)
  "Propertize the string S as a comment in a magit-diff buffer."
  (propertize (concat (magit-gh--format-comment-body s) "\n")
              'face 'magit-blame-highlight ;; TODO: Change this)
              ))

(defun magit-gh--display-threads (threads gh-diff-body)
  "Display comment threads in the magit-diff buffer.

Given a sequence of comment THREADS and Github diff GH-DIFF-BODY from
which they came, add them to current magit-diff buffer."
  (save-excursion
    (dolist (thread threads)
      (let ((magit-pos (magit-gh--diff-pos/gh->magit (magit-gh-comment-file (car thread))
                                                     (magit-gh-comment-gh-pos (car thread))
                                                     gh-diff-body))
            (ov))
        (magit-gh--visit-diff-pos (magit-gh-comment-file (car thread)) magit-pos)
        (setq ov (make-overlay (point-at-bol) (1+ (point-at-eol))))
        (overlay-put ov 'magit-gh-comment (car thread))
        (overlay-put ov 'face 'underline)
        (overlay-put ov 'after-string (magit-gh--propertize-overlay-text
                                       (mapcar (lambda (comment)
                                                 (format "%s - @%s\n"
                                                         (magit-gh-comment-text comment)
                                                         (magit-gh-comment-author comment)))
                                               thread)))))))

(defun magit-gh--delete-comment-overlays ()
  "Delete all comment overlays in the current buffer."
  (interactive)
  (let ((overlays (-filter (lambda (ov) (overlay-get ov 'magit-gh-comment))
                           (overlays-in (point-min) (point-max)))))
    (dolist (overlay overlays)
      (delete-overlay overlay))))

;; TODO: Assert that the current magit-gh-pull revision is up to date
;; with HEAD of the PR branch
(defun magit-gh--refresh-comments ()
  "Refresh comments for the current PR."
  (interactive)
  (when (derived-mode-p 'magit-diff-mode)
    (with-current-buffer (current-buffer)
      (save-excursion
        (magit-gh--delete-comment-overlays)
        (-if-let* ((current-pr (magit-gh--get-current-pr))
                   (reviews (magit-gh--list-reviews current-pr))
                   (gh-diff (magit-gh--fetch-diff-from-github current-pr)))
            (dolist (review reviews)
              (magit-gh--display-threads (magit-gh-review-threads review) gh-diff)))))))

(add-hook 'magit-refresh-buffer-hook #'magit-gh--refresh-comments)
;; (remove-hook 'magit-refresh-buffer-hook #'magit-gh--refresh-comments)

(defun magit-gh--capture-current-pull-request ()
  "Create a magit-gh-pr from the PR at point.

Assumes we are in a magit-status buffer in the `Pull Requests'
section, looking at a section of type `pull' (created by
magit-gh-pulls)"
  (let ((section-val (oref (magit-current-section) value))
        (pr-data (magit-gh-section-req-data)))
    (cl-destructuring-bind (user proj id) section-val
      (make-magit-gh-pr :owner user
                        :repo-name proj
                        :pr-number id
                        :diff-range (concat (oref (oref pr-data :base) :sha)
                                            ".."
                                            (oref (oref pr-data :head) :sha))))))

(defun magit-gh--get-current-pr ()
  "Return the Pull Request or nil, if not set."
  (and (boundp 'magit-gh--current-pr)
       magit-gh--current-pr))

(defvar magit-gh--review-body-placeholder-text (propertize "Leave a comment"
                                                           'magit-gh-is-placeholder t
                                                           'face 'italic))

(defun magit-gh--get-review-body (section)
  (let ((section-content (magit-gh--section-content-as-string section)))
    (if (get-text-property 0 'magit-gh-is-placeholder section-content)
        nil
      (progn
        (set-text-properties 0 (length section-content) nil section-content)
        section-content))))


(defun magit-gh--sorted-ht-each (fn sort-key-fn table &optional sort-pred-fn)
  "Like `ht-each', but sort the entries before iterating over them.

Maps SORT-KEY-FN over the keys and orders the resulting list
according to SORT-PRED-FN. Then call FN for each key and value in
the sorted list of entries. FN should accept 2 arguments, a key
and value."
  (let ((sorted-keys (magit-gh--sort (ht-keys table)
                                     sort-key-fn
                                     sort-pred-fn)))
    (dolist (k sorted-keys)
      (funcall fn k (ht-get table k)))))

(ert-deftest test-magit-gh--sorted-ht-each ()
  (let ((tbl (ht-from-alist `(("a" . 1)
                              ("z" . 2)
                              ("c" . 3))))
        accum)
    (magit-gh--sorted-ht-each (lambda (k _) (push k accum)) #'identity tbl #'string<)
    (should (equal accum '("z" "c" "a"))))
  (let ((tbl (ht-from-alist `((1 . "a")
                              (2 . "z")
                              (3 . "c"))))
        accum)
    (magit-gh--sorted-ht-each (lambda (k _) (push k accum)) #'identity tbl)
    (should (equal accum '(3 2 1)))))

(defun magit-gh--insert-reviews (pr reviews diff-body)
  (dolist (review (magit-gh--sort reviews #'magit-gh-review-id))
    (unless (and (string-empty-p (magit-gh-review-body review))
                 (not (magit-gh-review-threads review)))
      (magit-insert-section (review review)
        (magit-insert-heading (format "Review by %s"
                                      (magit-gh-review-author review)))
        (if-let ((body (or (if (string-empty-p (magit-gh-review-body review))
                               nil
                             (magit-gh-review-body review))
                           (and (equal (magit-gh-review-state review) 'pending)
                                magit-gh--review-body-placeholder-text))))
            (magit-insert-section (review-body)
              (insert (magit-gh--text-with-keymap body
                                                  `(,(kbd "C-c '") . ,'magit-gh--edit-prose)))
              (insert "\n")))
        (dolist (thread (magit-gh-review-threads review))
          (letfn ((make-heading (lambda (comment)
                                  (format "%sComment at %s"
                                          (if (magit-gh-comment-is-outdated comment)
                                              "[Outdated] "
                                            "")
                                          "<timestamp>"))))
            (if (> (length thread) 1)
                (magit-insert-section (thread thread)
                  (magit-insert-heading (make-heading (car thread)))
                  (dolist (comment thread)
                    (magit-gh--insert-comment comment pr diff-body)))
              (dolist (comment thread)
                (magit-gh--insert-comment comment
                                          pr
                                          diff-body
                                          (make-heading comment))))))))))


;; TODO: Why do we have 2 functions for this? (we also have
;; magit-gh--display-threads for overlays in diff buffers - let's
;; consolidate)
(defun magit-gh--insert-comment (comment pr diff-body &optional heading)
  (magit-insert-section (comment comment (magit-gh-comment-is-outdated comment))
    (when heading
      (magit-insert-heading heading))
    (when (not (magit-gh-comment-in-reply-to comment))
      (insert (magit-gh--propertize-comment-ctx diff-body
                                                (magit-gh-pr-diff-range pr)
                                                comment))
      (insert "\n"))
    (insert (magit-gh--propertize-comment-text (magit-gh-comment-text comment)))
    (insert "\n")
    (insert (format "- %s" (magit-gh-comment-author comment)))
    (insert "\n")))

(defun magit-gh--propertize-comment-text (text)
  "Setup keybindings that allow quick reply-to"
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "R")
      (lambda (response-text)
        (interactive "MReply: ")
        (let* ((comment (oref (magit-current-section) value))
               (id (magit-gh-comment-id comment)))
          (magit-gh-reply-to-comment id response-text))))
    (propertize text 'keymap keymap)))

(defun magit-gh--propertize-comment-ctx (diff-body diff-range comment)
  (let* ((diff-body (if (not (magit-gh-comment-is-outdated comment))
                        diff-body
                      (magit-gh--fetch-diff-for-commit-from-github
                       (magit-gh--get-current-pr)
                       (magit-gh-comment-commit-sha comment))))
         (diff-range (if (not (magit-gh-comment-is-outdated comment))
                         diff-range
                       (let ((base-sha (car (split-string diff-range "\\.\\.")))
                             (original-head-sha (substring (magit-gh-comment-commit-sha comment) 0 6)))
                         (format "%s..%s" base-sha original-head-sha))))
         (position (if (not (magit-gh-comment-is-outdated comment))
                       (magit-gh-comment-gh-pos comment)
                     (magit-gh-comment-original-gh-pos comment)))
         (comment-ctx (magit-gh--comment-ctx
                       diff-body
                       position
                       (magit-gh-comment-file comment)))
         (ctx-lines (split-string comment-ctx "\n"))
         (i 0)
         (in-hunk t)
         result)
    (dolist (line (reverse ctx-lines)
                  (string-join result "\n"))
      (setq in-hunk (and in-hunk
                         (not (string-match-p magit-gh--hunk-header-re line))))
      (setq result
            (cons (if in-hunk
                      (propertize line
                                  'keymap
                                  (let ((keymap (make-sparse-keymap)))
                                    (define-key keymap (kbd "<return>")
                                      (let ((gh-pos (- position i)))
                                        (lambda ()
                                          (interactive)
                                          (let ((magit-pos (magit-gh--diff-pos/gh->magit
                                                            (magit-gh-comment-file comment)
                                                            gh-pos
                                                            diff-body)))
                                            (magit-diff diff-range)
                                            (magit-gh--visit-diff-pos (magit-gh-comment-file comment)
                                                                      magit-pos)))))
                                    keymap))
                    line)
                  result))
      (setq i (1+ i)))))

(defun magit-gh--max-width-region (start end)
  (interactive "r")
  (let ((lines (s-split "\n" (buffer-substring start end))))
    (apply #'max (mapcar #'length lines))))

(defun magit-gh--pad-region (start end width &optional filler-char)
  (interactive "r\nnWidth: ")
  (let ((lines (s-split "\n" (buffer-substring start end))))
    (s-join "\n"
            (mapcar (lambda (l)
                      (s-pad-right width
                                   (char-to-string (or filler-char ?\s))
                                   l))
                    lines))))

(defun magit-gh--comment-ctx (diff-body gh-pos file)
  (letfn ((walk-within-hunk (lambda (n)
                              "Walk N lines, stopping at hunk headers"
                              (when (looking-at-p magit-gh--hunk-header-re)
                                (error "Position must be inside a hunk!"))
                              (while (and (not (zerop n))
                                          (not (looking-at-p magit-gh--hunk-header-re)))
                                (forward-line (/ n (abs n)))
                                (setq n (- n (/ n (abs n))))))))
    (save-excursion
      (let* ((padded-diff (with-temp-buffer
                            (insert diff-body)
                            ;; TOOD: the diff often gets condensed
                            ;; when it's painted, so this looks too
                            ;; wide. We should probably use the width
                            ;; of the *painted* diff instead
                            (let ((max-width (magit-gh--max-width-region
                                              (point-min)
                                              (point-max))))
                              (magit-gh--pad-region (point-min)
                                                    (point-max)
                                                    max-width))))
             (painted-padded-diff (with-temp-buffer
                                    (insert padded-diff)
                                    (goto-char (point-min))
                                    (magit-gh--paint-diff)
                                    (goto-char (point-min))
                                    ;; strip text properties from the
                                    ;; \n characters so that the "margins" aren't painted
                                    (s-join "\n"
                                            (s-split "\n" (buffer-string))))))
        (magit-gh--with-temp-buffer
          (insert painted-padded-diff)
          (goto-char (point-min))
          (magit-gh--jump-to-file-in-diff file)
          (re-search-forward magit-gh--hunk-header-re)
          (forward-line gh-pos)
          (let* ((num-lines-ctx-before 3)
                 (beg (save-excursion (walk-within-hunk (* -1 num-lines-ctx-before))
                                      (point)))
                 (end (point-at-eol)))
            (format "%s\n%s" file (buffer-substring beg end))))))))

(ert-deftest test-magit-gh--format-diff-ctx ()
  (cl-flet* ((this-line () (buffer-substring (point-at-bol) (point-at-eol)))
             (text-with-face (face text) (propertize text 'face face))
             (assert-highlighted
              (face text)
              (and
               (should (equal-including-properties
                        (s-trim-right (this-line))
                        (text-with-face face text)))
               (should-not (get-text-property (point-at-eol) 'face)))))
    (let ((diff-body (s-dedent "\
                        diff --git a/magit-gh-comments-utils.el b/magit-gh-comments-utils.el
                        index f3841f0..ea223f7 100644
                        --- a/magit-gh-comments-utils.el
                        +++ b/magit-gh-comments-utils.el
                        @@ -26,7 +26,7 @@ The start and end of the overlays are inclusive.\"
                        
                        (defun magit-gh--delete-overlays-at-point ()
                        (interactive)
                        -  (dolist (ov (overlays-at-point))
                        +  (dolist (ov (magit-gh--overlays-at-point))
                        +  some other line
                            (delete-overlay ov)))
                        
                        ;; TODO: This probably doesn't belong here
")))
      (with-current-buffer (get-buffer-create "diff-test")
        (delete-region (point-min) (point-max))
        (insert (magit-gh--comment-ctx diff-body 6 "magit-gh-comments-utils.el"))
        (goto-char (point-min))
        (should (magit-gh--looking-at-p "^magit-gh-comments-utils.el$"))
        (should (= 0 (forward-line 2)))
        (assert-highlighted 'magit-diff-removed "-  (dolist (ov (overlays-at-point))")
        (should (= 0 (forward-line 1)))
        (assert-highlighted 'magit-diff-added "+  (dolist (ov (magit-gh--overlays-at-point))")
        (should (= 0 (forward-line 1)))
        (assert-highlighted 'magit-diff-added "+  some other line")))))

;; Review drafts

(defvar magit-gh--review-drafts (ht-create)
  "Map of pending reviews: magit-gh-pr -> magit-gh-review")

(defun magit-gh--store-review-draft (review pr)
  (ht-set! magit-gh--review-drafts pr review))

(defun magit-gh--get-review-draft (pr)
  (ht-get magit-gh--review-drafts pr))

(defun magit-gh--discard-review-draft (&optional pr)
  (interactive)
  (if (not pr)
      (ht-clear! magit-gh--review-drafts)
    (ht-remove! magit-gh--review-drafts pr)))

;;;###autoload
(defun magit-gh-start-review ()
  (interactive)
  (let ((pr (magit-gh--get-current-pr))
        (magit-generate-buffer-name-function #'magit-review--buffer-name))
    (magit-mode-setup-internal 'magit-review-mode (list pr) t)))

(provide 'magit-gh-comments-core)

;;; magit-gh-comments-core.el ends here
