;; -*- lexical-binding: t -*-

(require 'ert)

(require 'magit-gh-comments-github)

(defstruct magit-gh-diff-pos a-or-b hunk-start offset)
(defstruct magit-gh-comment file diff-pos text)

(defun magit-gh--cur-diff-pos ()
  "Calculate the position of point within a magit diff and return
a magit-gh-diff-pos."
  (interactive)
  (save-excursion
    (re-search-backward "^@\\{2,\\} \\(.+?\\) @\\{2,\\}\\(?: \\(.*\\)\\)?"))
  (let* ((heading (match-string 0))
         (irrelevant-section-heading (match-string 2))
         (hunk-ranges (split-string (match-string 1)))
         (rev-a-range (car hunk-ranges))
         (rev-b-range (cadr hunk-ranges))
         (target-line (line-number-at-pos))
         (offset 0)
         (is-looking-at-rev-a  (and rev-a-range rev-b-range
                                    (save-excursion (goto-char (line-beginning-position))
                                                    (looking-at "^-"))))
         (rev-range (if is-looking-at-rev-a
                        rev-a-range
                      rev-b-range))
         (hunk-start (progn
                       (string-match (format "^%s\\([0-9]+\\)" (if is-looking-at-rev-a "-" "\\+")) rev-range)
                       (string-to-number (match-string 1 rev-range)))))
    (save-excursion
      (re-search-backward "^@\\{2,\\} \\(.+?\\) @\\{2,\\}\\(?: \\(.*\\)\\)?")
      (forward-line)
      (beginning-of-line)
      (while (< (line-number-at-pos) target-line)
        (unless (looking-at
                 (format "^%s" (if is-looking-at-rev-a "\\+" "-")))
          (cl-incf offset))
        (assert (= 0 (forward-line))))
      (make-magit-gh-diff-pos :a-or-b (if is-looking-at-rev-a :a :b)
                              :hunk-start hunk-start
                              :offset offset))))

(defun foo () (interactive) (magit-section-forward))
(defun magit-gh--visit-diff-pos (file pos)
  "Put point on the beginning of the line corresponding to
magit-gh-diff-pos POS."
  (goto-char (point-min))
  (if-let ((file-section (car (-filter
                               (lambda (sec) (and (magit-file-section-p sec)
                                                  (string= (magit-section-value sec)
                                                           file)))
                               (oref magit-root-section children)))))
      ;; TODO: Use magit-next-section or some such thing
      (magit-section-goto file-section))
  ;; (re-search-forward (format "+++ b/%s" file)) ;; TODO: What about files that have been renamed?
  (let ((found-p nil))
    (while (not found-p)
      (forward-line)
      (-if-let* ((hunk-header (magit-gh--try-parse-hunk-header))
                 (rev (magit-gh-diff-pos-a-or-b pos))
                 (start (alist-get (if (eq :a rev) :a-start :b-start)
                                   hunk-header))
                 (len (alist-get (if (eq :a rev) :a-len :b-len)
                                 hunk-header))
                 (target (+ (magit-gh-diff-pos-offset pos)
                            (magit-gh-diff-pos-hunk-start pos)))
                 (i 0))
          (when (and (>= target start)
                     (<= target (+ start len)))
            (while (<= (+ start i) target)
              (assert (= 0 (forward-line)))
              (unless (looking-at
                 (format "^%s" (if (eq :a rev) "\\+" "-")))
                (cl-incf i)))
            (setq found-p t))))))

(defun magit-gh--try-parse-hunk-header (&optional line)
  "Read the current line as a hunk-header. Return nil if the line
at point is not a hunk-header"
  (interactive)
  (let ((line (or line (buffer-substring (point-at-bol) (point-at-eol)))))
    (with-temp-buffer
      (insert line)
      (goto-char (point-min))
      (if (looking-at "@@ -\\([0-9]+\\),\\([0-9]+\\) \\+\\([0-9]+\\),\\([0-9]+\\) @@")
          `((:a-start . ,(string-to-number (match-string 1)))
            (:a-len . ,(string-to-number (match-string 2)))
            (:b-start . ,(string-to-number (match-string 3)))
            (:b-len . ,(string-to-number (match-string 4))))))))

(defun magit-gh--partition-by (xs-in fn &optional xs-out)
  "Apply FN to each value in XS-IN, splitting it each time FN
returns a new value. Return a list of lists."
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
                                   (cons (list (car xs-in)) (cons (reverse (car xs-out)) (cdr xs-out))))))))

(ert-deftest test-magit-gh--partition-by ()
  (should (equal '((0 1) (2 3) (4 5))
                 (magit-gh--partition-by '(0 1 2 3 4 5) (lambda (n) (/ n 2)))))
  (should (equal '((:a :b :c) (:d))
                 (magit-gh--partition-by '(:a :b :c :d)  (lambda (x) (eq :d x))))))

(defun magit-gh--pair-and-merge (xs-in &optional xs-out)
  "Pair off adjacent sequences in XS-IN and merge their
contents. Return a sequence of sequences which is half the length
of the original.

Example:

'((:foo) (:bar) (:buzz) (:bop)) -> '((:foo :bar) (:buzz :bop))
"
  (assert (evenp (length xs-in)))
  (if (not xs-in)
      (reverse xs-out)
    (magit-gh--pair-and-merge (cddr xs-in) (cons (append (car xs-in) (cadr xs-in)) xs-out))))

(ert-deftest test-magit-gh--pair-and-merge ()
  (should (equal '((1 2 3 4) (5 6 7 8))
                 (magit-gh--pair-and-merge '((1) (2 3 4) (5 6) (7 8))))))

(defun magit-gh--running-sum (xs)
  "Return a sequence of the same length as XS, where the element
at each index i in the output equals the sum of XS[0..i]

Example:

'(1 2 0) -> '(1 3 3)
"
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
         (lines-preceding-target (seq-take-while (lambda (n) (< n (1+ magit-offset)))
                                                (magit-gh--running-sum
                                                 (mapcar (lambda (line)
                                                           (if (not (= char-to-ignore (elt line 0))) 1 0))
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
               (magit-gh--calc-gh-offset lines (funcall make-pos :a 0))))
    (should (= 2
               (magit-gh--calc-gh-offset lines (funcall make-pos :b 0))))
    (should (= 3
               (magit-gh--calc-gh-offset lines (funcall make-pos :b 1))))
    (should (= 3
               (magit-gh--calc-gh-offset lines (funcall make-pos :a 1))))
    (should (= 4
               (magit-gh--calc-gh-offset lines (funcall make-pos :a 2))))
    (should-error (magit-gh--calc-gh-offset lines (funcall make-pos :b 2)))
    (should-error (magit-gh--calc-gh-offset lines (funcall make-pos :a 3)))))


(defun magit-gh--diff-pos/magit->gh (pos diff-body)
  "Given magit diff position POS and a DIFF-BODY (of type
application/vnd.github.v3.diff, from Github), calculate the
corresponding Github-style position."
  (let* ((lines (split-string diff-body "\n" t))
         (lines (seq-drop-while
                 ;; discard everything before the first hunk header
                 (lambda (x) (not (magit-gh--try-parse-hunk-header x)))
                 lines))
         (lines-partitioned-by-hunk-header
          (magit-gh--partition-by lines 'magit-gh--try-parse-hunk-header))
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
  "Given a FILE and a Github-style diff position GH-POS in
GH-DIFF-BODY, return the corresponding magit-gh-diff-pos"
  (with-temp-buffer
    (save-excursion
      (insert gh-diff-body)
      (goto-char (point-min))
      (re-search-forward (format "+++ b/%s" file))
      (re-search-forward "@@ -\\([0-9]+\\),\\([0-9]+\\) \\+\\([0-9]+\\),\\([0-9]+\\) @@")
      (forward-line gh-pos)
      (magit-gh--cur-diff-pos))))


(defun magit-gh--current-section-content ()
  "Return the contents of the magit section at point as a string"
  (interactive)
  (buffer-substring-no-properties
   (magit-section-start (magit-current-section))
   (magit-section-end (magit-current-section))))


(defun magit-gh-add-comment (comment-text)
  "Comment on the line at point and post the comment to Github"
  (interactive "MComment: ")
  (let* ((lines (split-string (magit-gh--current-section-content) "\n" t))
         (magit-pos (magit-gh--cur-diff-pos)) ;; TODO: What if the line at point isn't part of a hunk/isn't changed by this diff?
         (diff-body (magit-gh--current-section-content))  ;; TODO: This should by the *github* diff, not the magit diff!
         (github-pos (magit-gh--diff-pos/magit->gh magit-pos diff-body))
         (commit-sha (cdr (magit-split-range (magit-diff--dwim)))))
    (magit-gh--post-pr-comment (magit-gh--get-current-pr)
                               (magit-current-file)
                               commit-sha
                               github-pos
                               comment-text)))

;; (setq magit-gh-test-pr-comments (magit-gh--list-comments magit-gh-comment-test-pr))
;; (setq magit-gh-test-pr-diff-body (with-current-buffer "pr-2-diffs" (buffer-substring-no-properties (point-min) (point-max))))
;; (setq magit-gh-sample-diff-pos
;;       (magit-gh--diff-pos/gh->magit "magit-gh-comments-github.el" 5
;;                                     (with-current-buffer  "pr-2-diffs"
;;                                       (buffer-substring-no-properties (point-min) (point-max)))))

;; TODO: Make nice customizable faces
(defgroup magit-gh-comments nil
  "Add comments to Github Pull Requests from magit"
  :group 'code)

(defun magit-gh--format-comment-body (s &optional fill-width)
  (with-temp-buffer
    (insert s)
    (let ((fill-column (or fill-width 80)))
      (fill-region (point-min) (point-max)))
    (goto-char (point-min))
    (while (re-search-forward "^" nil t)
      (replace-match ">>> "))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun magit-gh--propertize-overlay-text (s)
  (propertize (concat (magit-gh--format-comment-body s) "\n")
              'face 'magit-blame-highlight ;; TODO: Change this)
              ))

(defun magit-gh--display-comments (comments gh-diff-body)
  (save-excursion
    (dolist (comment comments)
      (let ((magit-pos (magit-gh--diff-pos/gh->magit (alist-get :path comment)
                                                     (alist-get :position comment)
                                                     gh-diff-body))
            (ov))
        (magit-gh--visit-diff-pos (alist-get :path comment) magit-pos)
        (setq ov (make-overlay (point-at-bol) (1+ (point-at-eol))))
        (overlay-put ov 'magit-gh-comment comment)
        (overlay-put ov 'face 'underline)
        (overlay-put ov 'after-string (magit-gh--propertize-overlay-text
                                       (alist-get :body comment)))))))

(defun magit-gh--delete-comment-overlays ()
  "Delete all comment overlays in the current buffer"
  (interactive)
  (let ((overlays (filter (lambda (ov) (overlay-get ov 'magit-gh-comment))
                          (overlays-in (point-min) (point-max)))))
    (dolist (overlay overlays)
      (delete-overlay overlay))))

(defun magit-gh-refresh-comments ()
  "Refresh the comments for the current PR"
  (interactive)
  (magit-gh--delete-comment-overlays)
  (-if-let* ((current-pr (magit-gh--get-current-pr))
             (comments (magit-gh--list-comments current-pr))
             (gh-diff (magit-gh--fetch-diff-from-github current-pr)))
      (magit-gh--display-comments comments gh-diff)
    (error "Couldn't fetch the PR associated with this diff! (This is likely a bug)")))


;;; Capture and store the associated PR when the user views its diff
;;; from the magit Pull Requests section
(define-advice magit-gh-pulls-diff-pull-request (:around (original-fn) capture-current-pull-request)
  (let ((section-val (magit-section-value (magit-current-section))))
    (funcall original-fn)
    (destructuring-bind (user proj id) section-val
      (setq-local magit-gh--current-pr (make-magit-gh-pr :owner user
                                                         :repo-name proj
                                                         :pr-number id)))))

(defun magit-gh--get-current-pr ()
  "Return the Pull Request object which the user is currently
viewing"
  (and (boundp 'magit-gh--current-pr)
       magit-gh--current-pr))

(provide 'magit-gh-comments)

;;; BEGIN - Debugging utilities - delete me

(defun overlays-at-point ()
  "Return overlays which touch point, with inclusive start and
end"
  (interactive)
  (filter (lambda (ov) (and (>= (point) (overlay-start ov))
                            (<= (point) (overlay-end ov))))
          (-flatten (overlay-lists))))


(defun delete-overlays-at-point ()
  (interactive)
  (dolist (ov (overlays-at-point))
    (delete-overlay ov)))

;;; END - Debugging utilities - delete me
