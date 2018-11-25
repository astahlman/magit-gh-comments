;; -*- lexical-binding: t -*-

(require 'ert)
(require 'magit-gh-comments)
;; (require 'magit-gh-comments-diff)

;; For running tests interactively
;; (load "test/test-helper")

(ert-deftest test-magit->gh/boundary-conditions ()
  (let* ((diff-body "diff --git a/f b/f
index 9fda99d..f88549a 100644
--- a/f
+++ b/f
@@ -1,3 +1,2 @@
 1. :a/1/1, :b/1/1
-2. :a/1/2
 3. :a/1/3, :b/1/2
\\ No newline at end of file")
         (diff-buff (magit-gh--put-in-buffer diff-body))
         (magit->gh (lambda (a-or-b offset)
                      (magit-gh--diff-pos/magit->gh
                       "f"
                       (make-magit-gh-diff-pos :a-or-b a-or-b
                                               :hunk-start 1
                                               :offset offset)
                       diff-body))))
    (should (= 1 (funcall magit->gh :b 1)))
    (should (= 1 (funcall magit->gh :a 1)))
    (should (= 2 (funcall magit->gh :a 2)))
    (should (= 3 (funcall magit->gh :a 3)))
    (should (= 3 (funcall magit->gh :b 2)))))


(ert-deftest test-magit-gh--diff-pos/magit->gh ()
  (let* ((rev-files (magit-gh--generate-revisions))
         (magit-diff-buf (magit-gh--generate-magit-diff (car rev-files) (cdr rev-files)))
         (random-diff-pos (magit-gh--pick-random-diff-pos magit-diff-buf))
         (diff-pos (alist-get :diff-pos random-diff-pos))
         (expected-line-contents (alist-get :line-contents random-diff-pos))
         (github-diff-body (with-current-buffer (magit-gh--generate-github-diff)
                             (buffer-substring-no-properties (point-min) (point-max))))
         (github-diff-pos (magit-gh--diff-pos/magit->gh (cdr rev-files)
                                                        diff-pos
                                                        github-diff-body))
    (should (string= expected-line-contents
                     (magit-gh--line-contents-at-github-pos github-diff-pos
                                                            magit-diff-buf))))))

;; TODO:
;; 1. Post a comment on line L in a diff
;; 2. Close the buffer
;; 3. Re-open the diff buffer
;; 4. Assert that the comment is present on line L


(defmacro with-mocks (fn-defs &rest body)
  (declare (indent 1) (debug ((&rest (sexp function-form)) body)))
  (let ((bindings (mapcar
                   (lambda (fn-def) `((symbol-function (quote ,(car fn-def))) ,(cadr fn-def)))
                   fn-defs)))
    `(cl-letf ,bindings
       ,@body)))

;; TODO: Move this definition out of the test, maybe reverse the
;; direction of the alias (or just remove it)
(defalias 'letfn #'with-mocks)

(setq magit-gh--test-pr
      (make-magit-gh-pr :owner "astahlman"
                        :pr-number 0
                        :repo-name "magit-gh-comments"
                        :diff-range "abcdef"))

(setq magit-gh--test-review-response
      '(((id . 42)
         (user (login . "astahlman"))
         (body . "This is a top-level review with a \360\237\220\250 thrown in for good measure. Here's a code sample, too:

```
def foo():
  print \"hi\"
```")
         (state . "COMMENTED")
         (submitted_at . "2018-09-02T16:57:24Z")
         (commit_id . "562b1f07ede9c579ae5ec2d79a07879dd7a0d031"))
        ((id . 43)
         (user (login . "spiderman"))
         ;; NOTE: Spiderman did not leave a top-level review
         (state . "COMMENTED")
         (submitted_at . "2018-09-02T17:00:00Z")
         (commit_id . "562b1f07ede9c579ae5ec2d79a07879dd7a0d031"))))

(setq magit-gh--test-comments-response
      '(((pull_request_review_id . 42)
         (body . "A comment about the removal of line 2")
         (path . "f")
         (position . 2)
         (user (login . "astahlman")))
        ((pull_request_review_id . 43)
         (body . "A comment about the addition of line 15")
         (path . "f")
         (position . 9)
         (user (login . "spiderman")))))

(setq magit-gh--test-diff-body "diff --git a/f b/f
index 9fda99d..f88549a 100644
--- a/f
+++ b/f
@@ -1,3 +1,2 @@
 1. :a/1/1, :b/1/1
-2. :a/1/2
 3. :a/1/3, :b/1/2
@@ -11,3 +10,5 @@
 11. :a/11/1, :b/10/1
 12. :a/11/2, :b/10/2
+13. :b/10/3
 14. :a/11/3, :b/10/4
+15. :b/10/5
\\ No newline at end of file
")


(defun mock-github-api (url &rest request-args)
  (cond ((equalp url "https://api.github.com/repos/astahlman/magit-gh-comments/pulls/0/comments")
         magit-gh--test-comments-response)
        ((equalp url "https://api.github.com/repos/astahlman/magit-gh-comments/pulls/0/reviews")
         magit-gh--test-review-response)
        ((and (equalp url "https://api.github.com/repos/astahlman/magit-gh-comments/pulls/0")
               ;; TODO: check that the args contain `:headers ("Accept" . "application/vnd.github.v3.diff")`
               ;;(equalp request-args )
              t)
         magit-gh--test-diff-body)
        (t (error "There is no mock configured for that request with URL `%s`" url))))

(ert-deftest test-magit-gh--github-fetch-reviews ()
  (with-mocks ((magit-gh--request-sync-internal #'mock-github-api))
    (let* ((response (magit-gh--list-reviews magit-gh--test-pr))
           (first-review (car response))
           (first-comment (car (alist-get :comments first-review))))
      (should (string-match-p "A comment about the removal of line 2"
                              (alist-get :body first-comment)))
      (should (string-match-p "This is a top-level review"
                              (alist-get :body first-review))))))


(ert-deftest test-magit-gh--github-fetch-comments ()
  ;; TODO: Test against the comment context given this diff body
  (let* ((diff-body magit-gh--test-diff-body))
    (with-mocks ((magit-gh--request-sync-internal #'mock-github-api))
                (should (equal '(((:pull_request_review_id . 42)
                                  (:author . "astahlman")
                                  (:body . "A comment about the removal of line 2")
                                  (:path . "f")
                                  (:position . 2))
                                 ((:pull_request_review_id . 43)
                                  (:author . "spiderman")
                                  (:body . "A comment about the addition of line 15")
                                  (:path . "f")
                                  (:position . 9)))
                               (magit-gh--list-comments magit-gh--test-pr))))))

;; (setq test-reviews (magit-gh--list-reviews magit-gh--test-pr))

;; TODO: Delete?
(defun magit-gh--jump-to-file-section (file)
  (interactive "sFile name: ")
  (goto-char (point-min))
  (let (foundp)
    (while (not foundp)
      (magit-section-when
          (and (equalp file (oref it type))
               (equalp (format "b/%s" file)
                       (oref it value)))
        (setq foundp t))
      (when (not foundp)
        (magit-section-forward)))))


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
           (magit-gh--with-temp-buffer
             (insert diff-body)
             (goto-char (point-min))
             (magit-gh--paint-diff)
             (goto-char (point-min))
             (magit-gh--jump-to-file-in-diff file)
             (re-search-forward magit-gh--hunk-header-re)
             (forward-line gh-pos)
             (let* ((num-lines-ctx-before 3)
                    (beg (save-excursion (walk-within-hunk (* -1 num-lines-ctx-before))
                                         (point)))
                    (end (save-excursion (end-of-line)
                                         (point))))
               (format "%s\n%s" file (buffer-substring beg end)))))))

(ert-deftest magit-gh--test-comment-ctx ()
  (let ((diff-body "diff --git a/f b/f
index 9fda99d..f88549a 100644
--- a/f
+++ b/f
@@ -1,3 +1,2 @@
 1. :a/1/1, :b/1/1
-2. :a/1/2
 3. :a/1/3, :b/1/2
@@ -11,2 +10,3 @@
 11. :a/11/1, :b/10/1
 12. :a/11/2, :b/10/2
+13. :b/10/3
 14. :a/11/3, :b/10/4
+15. :b/10/5
\\ No newline at end of file
"))
    (should (equalp "f
@@ -1,3 +1,2 @@
 1. :a/1/1, :b/1/1
-2. :a/1/2
 3. :a/1/3, :b/1/2"
                    (magit-gh--str-without-props
                     (magit-gh--comment-ctx diff-body 3 "f"))))
    (should (equalp "f
@@ -1,3 +1,2 @@
 1. :a/1/1, :b/1/1
-2. :a/1/2"
                    (magit-gh--str-without-props
                     (magit-gh--comment-ctx diff-body 2 "f"))))
    (should (equalp "f
@@ -1,3 +1,2 @@
 1. :a/1/1, :b/1/1"
                    (magit-gh--str-without-props
                     (magit-gh--comment-ctx diff-body 1 "f"))))
    (should (equalp "f
@@ -11,2 +10,3 @@
 11. :a/11/1, :b/10/1
 12. :a/11/2, :b/10/2
+13. :b/10/3"
                    (magit-gh--str-without-props
                     (magit-gh--comment-ctx diff-body 7 "f"))))
    (should (equalp "f
@@ -11,2 +10,3 @@
 11. :a/11/1, :b/10/1
 12. :a/11/2, :b/10/2"
                    (magit-gh--str-without-props
                     (magit-gh--comment-ctx diff-body 6 "f"))))
    (should (equalp "f
@@ -11,2 +10,3 @@
 11. :a/11/1, :b/10/1"
                    (magit-gh--str-without-props
                     (magit-gh--comment-ctx diff-body 5 "f"))))
    (should-error (magit-gh--comment-ctx diff-body 4 "f"))))

(ert-deftest magit-gh--test-populate-reviews-buffer ()
  (let ((magit-gh--current-pr magit-gh--test-pr)
        magit-diff-calls
        visit-diff-pos-calls)
    (with-mocks ((magit-gh--request-sync-internal #'mock-github-api)
                 (magit-diff (lambda (&rest args)
                               (setq magit-diff-calls
                                     (cons args magit-diff-calls))))
                 (magit-gh--visit-diff-pos (lambda (&rest args)
                                             (setq visit-diff-pos-calls
                                                   (cons args visit-diff-pos-calls)))))
      (magit-gh--populate-reviews magit-gh--test-pr)
      (switch-to-buffer "magit-pull-request: magit-gh-comments/0")
      (goto-char (point-min))
      (should (magit-gh--looking-at-p "FIXME - PR TITLE HERE"))
      ;; Review sections
      (magit-section-forward)
      (should (magit-gh--looking-at-p "Review by astahlman"))
      (should (s-contains? "This is a top-level review"
                           (magit-gh--section-content-as-string)))
      (magit-section-forward)
      (should (magit-gh--looking-at-p "f
@@ -1,3 +1,2 @@
 1. :a/1/1, :b/1/1
-2. :a/1/2
A comment about the removal of line 2
- astahlman"))
      (forward-line 3)
      (should (magit-gh--looking-at-p "-2. :a/1/2"))
      (save-excursion
        (execute-kbd-macro (kbd "<return>"))
        (should (equal (car visit-diff-pos-calls)
                       `("f" ,(make-magit-gh-diff-pos :a-or-b :a
                                                      :hunk-start 1
                                                      :offset 2))))
        (should (equal (caar magit-diff-calls)
                       (magit-gh-pr-diff-range magit-gh--test-pr))))
      (magit-section-forward-sibling)
      (should (magit-gh--looking-at-p "Review by spiderman"))
      (magit-section-forward)
      (should (magit-gh--looking-at-p "f
 12. :a/11/2, :b/10/2
+13. :b/10/3
 14. :a/11/3, :b/10/4
+15. :b/10/5
A comment about the addition of line 15
- spiderman"))
      (forward-line 4)
      (should (magit-gh--looking-at-p "+15. :b/10/5"))
      (save-excursion
        (execute-kbd-macro (kbd "<return>"))
        (should (equal (car visit-diff-pos-calls)
                       `("f" ,(make-magit-gh-diff-pos :a-or-b :b
                                                      :hunk-start 10
                                                      :offset 5))))))))
