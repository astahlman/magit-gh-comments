;;; magit-gh-comments.el --- Comment on Github Pull Requests via Magit  -*- lexical-binding: t; coding: utf-8 -*-

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

(require 'cl)
(require 'ert)

(require 'magit-gh-comments-github)

(defstruct magit-gh-diff-pos a-or-b hunk-start offset)

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


(defun magit-gh--translate-diff-pos (pos &optional diff-body)
  "Given magit diff position POS, calculate the corresponding
Github-style position."
  (let* ((lines (split-string
                 (or diff-body
                     (buffer-substring-no-properties (point-min) (point-max)))
                 "\n" t))
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
         (magit-pos (magit-gh--cur-diff-pos))
         (github-pos (magit-gh--translate-diff-pos magit-pos (magit-gh--current-section-content)))
         (commit-sha (magit-diff-visit--range-end)))
    (magit-gh--post-pr-comment magit-gh-comment-test-pr
                               (magit-current-file)
                               commit-sha
                               github-pos
                               comment-text)))

(provide 'magit-gh-comments)

;;; magit-gh-comments.el ends here
