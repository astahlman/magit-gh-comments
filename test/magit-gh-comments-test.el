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


(defalias 'with-mocks #'letfn)

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
         (user (login . "spiderman")))
        ((pull_request_review_id . 43)
         (body . "Some other comment that's been resolved")
         (path . "f")
         (position . nil)
         (original_position . 10)
         (original_commit_id . "abcdef")
         (user (login . "spiderman")))))

(setq

 ;; HEAD~1
 magit-gh--test-diff-body-prev-commit "diff --git a/f b/f
index 9fda99d..abcdef 100644
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
+16. :b/10/6 (this line will be deleted in the next commit)
\\ No newline at end of file
"
 ;; HEAD
 magit-gh--test-diff-body "diff --git a/f b/f
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


(setq magit-gh--test-pr-response
      '((body . "Fake PR description here
With a carriage-return + line-feed.")
        (title . "PR Title")
        (state . "open")))

(defun mock-github-api (url &rest request-args)
  (cond ((equalp url "https://api.github.com/repos/astahlman/magit-gh-comments/pulls/0/comments")
         magit-gh--test-comments-response)
        ((equalp url "https://api.github.com/repos/astahlman/magit-gh-comments/pulls/0/reviews")
         magit-gh--test-review-response)
        ((and (equalp url "https://api.github.com/repos/astahlman/magit-gh-comments/pulls/0")
              (equalp "application/vnd.github.v3.diff"
                      (magit-gh--extract-header "Accept" request-args)))
         magit-gh--test-diff-body)
        ((equalp url "https://api.github.com/repos/astahlman/magit-gh-comments/pulls/0")
         magit-gh--test-pr-response)
        ((equalp url "https://api.github.com/repos/astahlman/magit-gh-comments/commits/abcdef")
         magit-gh--test-diff-body-prev-commit)
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

(defun magit-gh--discard-empty-keys (l)
  (let (result)
    (dolist (pair l result)
      (when (cdr pair)
        (setq result (cons pair result))))))


(defun alist-equal-p (a1 a2)
  (letfn ((k-str (lambda (a)
                   (if (symbolp a)
                       (symbol-name a)
                     (string a)))))
    (cond ((and (not a1) (not a2))
           t)
          ((and (atom a1) (atom a2))
           (equal a1 a2))
          ((and (listp a1) (listp a2)
                (atom (cdr a1)) (atom (cdr a2)))
           (and (alist-equal-p (car a1) (car a2))
                (alist-equal-p (cdr a1) (cdr a2))))
          ((and (listp a1) (listp a2))
           (let ((s1 (sort a1 (lambda (p1 p2) (string< (k-str (car p1)) (k-str (car p2))))))
                 (s2 (sort a2 (lambda (p1 p2) (string< (k-str (car p1)) (k-str (car p2)))))))
             (and (alist-equal-p (car s1) (car s2))
                  (alist-equal-p (cdr s1) (cdr s2)))))
          (t nil))))

(defun alist-equal-p (a1 a2)
  (letfn ((alistp (lambda (x) (and (listp x) (-every? #'listp x)))))
    (cond ((equal a1 a2) t)
          ((and (alistp a1) (alistp a2))
           (catch 'not-equal
             (and
              (dolist (kv a1 t)
                (unless (alist-equal-p (alist-get (car kv) a1)
                                       (alist-get (car kv) a2))
                  (throw 'not-equal nil)))
              (dolist (kv a2 t)
                (unless (alist-equal-p (alist-get (car kv) a1)
                                       (alist-get (car kv) a2))
                  (throw 'not-equal nil))))))
          (t nil))))

(ert-deftest test-alist-equal-p ()
  "Test of our test infrastructure"
  (should (alist-equal-p '((:a . 1) (:b . 2))
                         '((:b . 2) (:a . 1))))
  (should (not (alist-equal-p '((:a . 2) (:b . 1))
                              '((:a . 1) (:b . 2)))))
  (should (alist-equal-p '((:a . 1) (:b . 2))
                         '((:b . 2) (:a . 1) (:c . nil))))
  (should (not (alist-equal-p '((:a . 1) (:b . 2))
                              '((:b . 2) (:a . 1) (:c . 3)))))
  (should (alist-equal-p '((:a . 1) (:b . (1 2 3)))
                         '((:b . (1 2 3)) (:a . 1))))
  (should (not (alist-equal-p '((:a . 1) (:b . (1 2 3 "foo")))
                              '((:b . (1 2 3)) (:a . 1)))))
  (should (alist-equal-p '((:a . 1) (:b . (1 2 3 "foo")))
                               '((:b . (1 2 3 "foo")) (:a . 1)))))

(ert-deftest test-magit-gh--github-fetch-comments ()
  ;; TODO: Test against the comment context given this diff body
  (let* ((diff-body magit-gh--test-diff-body)
         (sort-pred (lambda (x y)
                      (if (equal (alist-get :pull_request_review_id x)
                                 (alist-get :pull_request_review_id y))
                          (string< (alist-get :body x)
                                   (alist-get :body y))
                        (< (alist-get :pull_request_review_id x)
                           (alist-get :pull_request_review_id y)))))
         (retrieved-comments (with-mocks ((magit-gh--request-sync-internal #'mock-github-api))
                               (magit-gh--list-comments magit-gh--test-pr)))
         (retrieved-comments (sort (mapcar #'magit-gh--discard-empty-keys
                                           retrieved-comments)
                                   sort-pred))
         (expected-comments '(((:pull_request_review_id . 42)
                               (:author . "astahlman")
                               (:body . "A comment about the removal of line 2")
                               (:path . "f")
                               (:position . 2))
                              ((:pull_request_review_id . 43)
                               (:author . "spiderman")
                               (:body . "A comment about the addition of line 15")
                               (:path . "f")
                               (:position . 9))
                              ((:pull_request_review_id . 43)
                               (:author . "spiderman")
                               (:body . "Some other comment that's been resolved")
                               (:path . "f")
                               (:is_outdated . t)
                               (:original_position . 10)
                               (:original_commit_id . "abcdef")))))
    (should (= (length expected-comments) (length retrieved-comments)))
    (should (cl-every #'alist-equal-p expected-comments retrieved-comments))))

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
  (setq magit-gh--request-cache nil)
  (let ((magit-gh--current-pr magit-gh--test-pr)
        (expected-buf-name "magit-gh-comments: magit-gh-comments/0")
        magit-diff-calls
        visit-diff-pos-calls)
    (with-mocks ((magit-gh--request-sync-internal #'mock-github-api)
                 (magit-diff (lambda (&rest args)
                               (setq magit-diff-calls
                                     (cons args magit-diff-calls))))
                 (magit-git-insert (lambda (&rest args)
                                     ;; TODO: a dedent function would be nice, here
                                     (insert (s-dedent "\
                                                a                |  5 +++--
                                                another-file     |  7 +++++++
                                                2 files changed, 10 insertions(+), 2 deletions(-)") )))
                 (magit-gh--visit-diff-pos (lambda (&rest args)
                                             (setq visit-diff-pos-calls
                                                   (cons args visit-diff-pos-calls)))))
      (when-let ((buf (get-buffer expected-buf-name)))
        (kill-buffer buf))
      (magit-gh-show-reviews magit-gh--test-pr)
      (goto-char (point-min))
      ;; PR Title and description
      (should (magit-gh--looking-at-p (regexp-quote "PR Title (#0) [OPEN]")))
      (forward-line 2)
      (should (magit-gh--looking-at-p (regexp-quote "Fake PR description here
With a carriage-return + line-feed.")))
      ;; Diffstat
      (magit-section-forward)
      (should (magit-gh--looking-at-p "Files changed"))
      (magit-section-forward)
      (should (magit-gh--looking-at-p (s-dedent "\
                                        [0-9]+ files changed, [0-9]+ insertions(\\+), [0-9]+ deletions(-)
                                        \\([a-zA-Z-/]+ +| +[0-9] [+-]+
                                        ?\\)+")))
      (magit-section-forward) ;; file1
      (magit-section-forward) ;; file2
      ;; Review sections
      (magit-section-forward)
      (should (magit-gh--looking-at-p "Review by astahlman"))
      (should (s-contains? "This is a top-level review"
                           (magit-gh--section-content-as-string)))
      (magit-section-forward)
      (should (magit-gh--looking-at-p "Comment at .+
f
@@ -1,3 \\+1,2 @@
 1\\. :a/1/1, :b/1/1
-2\\. :a/1/2
A comment about the removal of line 2
- astahlman"))
      (forward-line 4)
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
      (should (oref (magit-current-section) hidden))
      (magit-section-toggle (magit-current-section))
      (should (magit-gh--looking-at-p "\\[Outdated\\] Comment at .+
f
\\+13\\. :b/10/3
 14\\. :a/11/3, :b/10/4
\\+15\\. :b/10/5
\\+16\\. :b/10/6 (this line will be deleted in the next commit)
Some other comment that's been resolved
- spiderman"))
      (magit-section-forward)
      (should (magit-gh--looking-at-p "Comment at .+
f
 12\\. :a/11/2, :b/10/2
\\+13\\. :b/10/3
 14\\. :a/11/3, :b/10/4
\\+15\\. :b/10/5
A comment about the addition of line 15
- spiderman"))
      (forward-line 5)
      (should (magit-gh--looking-at-p "\\+15\\. :b/10/5"))
      (save-excursion
        (execute-kbd-macro (kbd "<return>"))
        (should (equal (car visit-diff-pos-calls)
                       `("f" ,(make-magit-gh-diff-pos :a-or-b :b
                                                      :hunk-start 10
                                                      :offset 5))))))))

(ert-deftest magit-gh--test-fetch-review-draft ()
  (let* ((saved-comment (make-magit-gh-comment :file "f"
                                               :diff-pos (make-magit-gh-diff-pos :a-or-b :b
                                                                                 :hunk-start 1
                                                                                 :offset 2)
                                               :text "This comment isn't submitted yet"))
         (pending-review (make-magit-gh-review :body "I'm still working on this review"
                                               :comments (list saved-comment))))
    (magit-gh--store-review-draft pending-review magit-gh--test-pr)
    (should (equal pending-review
                   (magit-gh--get-review-draft magit-gh--test-pr)))))


(defun magit-gh--simulate-adding-comments (comments)
  (dolist (comment comments)
    (with-mocks ((magit-gh--get-current-pr (lambda () magit-gh--test-pr))
                 (magit-gh--cur-magit-diff-pos (lambda () (magit-gh-comment-diff-pos comment)))
                 (magit-current-file (lambda () (magit-gh-comment-file comment)))
                 (magit-diff--dwim (lambda () (magit-gh-pr-diff-range magit-gh--test-pr))))
      (magit-gh-add-comment nil (magit-gh-comment-text comment)))))

(setq magit-gh--test-comments
      (let ((comment1 (make-magit-gh-comment :file "a"
                                             :diff-pos (make-magit-gh-diff-pos :a-or-b :b
                                                                               :hunk-start 1
                                                                               :offset 2)
                                             :text "Comment 1"))
            (comment2 (make-magit-gh-comment :file "a"
                                             :diff-pos (make-magit-gh-diff-pos :a-or-b :b
                                                                               :hunk-start 1
                                                                               :offset 2)
                                             :text "Comment 2")))
        (list comment1 comment2)))

(ert-deftest magit-gh--test-add-pending-comments ()
  (magit-gh--discard-review-draft magit-gh--test-pr)
  (magit-gh--simulate-adding-comments magit-gh--test-comments)
  (should (equal (magit-gh-review-comments (magit-gh--get-review-draft magit-gh--test-pr))
                 (reverse magit-gh--test-comments))))


(ert-deftest magit-gh--test-submit-pending-comments ()
  (magit-gh--discard-review-draft magit-gh--test-pr)
  (let (mock-calls)
    (with-mocks ((magit-gh--get-current-pr (lambda () magit-gh--test-pr))
                 (magit-gh--post-review (lambda (&rest args) (push args mock-calls))))
      (magit-gh--simulate-adding-comments magit-gh--test-comments)
      (magit-gh--submit-pending-review "Super-great job")
      (should (equal (list magit-gh--test-pr
                           (make-magit-gh-review :body "Super-great job"
                                                 :comments (reverse magit-gh--test-comments)))
                     (car mock-calls))))))

(ert-deftest magit-gh--test-submission-rejected-if-empty ()
  (magit-gh--discard-review-draft magit-gh--test-pr)
  (with-mocks ((magit-gh--get-current-pr (lambda () magit-gh--test-pr)))
    (should-error (magit-gh--submit-pending-review) :type 'user-error)))

