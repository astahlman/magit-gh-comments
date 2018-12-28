;; -*- lexical-binding: t -*-

(require 'ert)
(require 'subr-x)
(require 'magit-gh-comments-core)
(require 'magit-pull-request)
(require 'magit-review)

;; For running tests interactively
(load-file (expand-file-name "test-helper.el"
                             (file-name-directory (or load-file-name buffer-file-name))))

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
                        :diff-range "abcdef..ghijkl"))

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
           (first-comment (car (magit-gh-review-comments first-review))))
      (should (string-match-p "A comment about the removal of line 2"
                              (magit-gh-comment-text first-comment)))
      (should (string-match-p "This is a top-level review"
                              (magit-gh-review-body first-review))))))

(defun magit-gh--discard-empty-keys (l)
  (let (result)
    (dolist (pair l result)
      (when (cdr pair)
        (setq result (cons pair result))))))

(ert-deftest test-magit-gh--github-fetch-comments ()
  ;; TODO: Test against the comment context given this diff body
  (let* ((diff-body magit-gh--test-diff-body)
         (sort-pred (lambda (x y)
                      (if (equal (magit-gh-comment-review-id x)
                                 (magit-gh-comment-review-id y))
                          (string< (magit-gh-comment-text x)
                                   (magit-gh-comment-text y))
                        (< (magit-gh-comment-review-id x)
                           (magit-gh-comment-review-id y)))))
         (retrieved-comments (with-mocks ((magit-gh--request-sync-internal #'mock-github-api))
                               (magit-gh--list-comments magit-gh--test-pr)))
         (retrieved-comments (sort retrieved-comments sort-pred))
         (expected-comments (list (make-magit-gh-comment :review-id 42
                                                         :author "astahlman"
                                                         :text "A comment about the removal of line 2"
                                                         :file "f"
                                                         :gh-pos 2)
                                  (make-magit-gh-comment :review-id 43
                                                         :author "spiderman"
                                                         :text "A comment about the addition of line 15"
                                                         :file "f"
                                                         :gh-pos 9)
                                  (make-magit-gh-comment :review-id 43
                                                         :author "spiderman"
                                                         :text "Some other comment that's been resolved"
                                                         :file "f"
                                                         :is-outdated t
                                                         :original-gh-pos 10
                                                         :commit-sha "abcdef"))))
    (should (equal expected-comments retrieved-comments))))

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

(defun magit-gh--section-search-forward (pred)
  (while (progn (condition-case nil
                    (magit-section-forward)
                  (user-error (error "End-of-buffer reached; no matching magit-section")))
                (not (funcall pred (magit-current-section))))))

(defun magit-gh--section-type-matcher (target-type)
  (lambda (section)
    (equal (oref section type) target-type)))


(ert-deftest magit-gh--test-populate-reviews-buffer ()
  (setq magit-gh--request-cache nil)
  (let ((magit-gh--current-pr magit-gh--test-pr)
        (expected-buf-name "PR: magit-gh-comments (#0)")
        magit-diff-calls
        visit-diff-pos-calls)
    (with-mocks ((magit-gh--request-sync-internal #'mock-github-api)
                 (magit-diff (lambda (&rest args)
                               (setq magit-diff-calls
                                     (cons args magit-diff-calls))))
                 (magit-gh--get-current-pr (lambda () magit-gh--test-pr))
                 (magit-git-insert (lambda (&rest args)
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
      (should (string= expected-buf-name (buffer-name)))
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
      (dotimes (_i 2)
        (magit-gh--section-search-forward
         (magit-gh--section-type-matcher 'file)))
      ;; Review sections
      (magit-gh--section-search-forward
       (magit-gh--section-type-matcher 'review))
      (should (magit-gh--looking-at-p "Review by astahlman"))
      (should (s-contains? "This is a top-level review"
                           (magit-gh--section-content-as-string)))
      (magit-gh--section-search-forward
       (magit-gh--section-type-matcher 'comment))
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
      (magit-gh--section-search-forward
       (magit-gh--section-type-matcher 'review))
      (should (magit-gh--looking-at-p "Review by spiderman"))
      (magit-gh--section-search-forward
       (magit-gh--section-type-matcher 'comment))
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
      (magit-gh--section-search-forward
       (magit-gh--section-type-matcher 'comment))
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

(ert-deftest test-magit-gh--skip-cache-on-reload-pull-request ()
  (setq magit-gh--request-cache nil)
  (let ((call-counts (ht-create)))
    (with-mocks ((magit-gh--get-current-pr (lambda () magit-gh--test-pr))
                 (magit-gh--request-sync-internal
                  (lambda (url &rest request-args)
                    (ht-set! call-counts url (1+ (ht-get call-counts url 0)))
                    (apply #'mock-github-api url request-args))))
      ;; TODO: Rename this to show PR?
      (magit-gh-show-reviews magit-gh--test-pr)
      ;; once to hydrate the PR, once to fetch diff
      (should (= 2 (ht-get call-counts (magit-gh--url-for-pr magit-gh--test-pr) 0)))
      (should (= 1 (ht-get call-counts (magit-gh--url-for-pr-reviews magit-gh--test-pr) 0)))
      (should (= 1 (ht-get call-counts (magit-gh--url-for-pr-comments magit-gh--test-pr) 0)))
      (magit-pull-request-reload-from-github)
      (should (= 4 (ht-get call-counts (magit-gh--url-for-pr magit-gh--test-pr) 0)))
      (should (= 2 (ht-get call-counts (magit-gh--url-for-pr-reviews magit-gh--test-pr) 0)))
      (should (= 2 (ht-get call-counts (magit-gh--url-for-pr-comments magit-gh--test-pr) 0))))))

(ert-deftest magit-gh--test-fetch-review-draft ()
  (let* ((saved-comment (make-magit-gh-comment :file "f"
                                               :gh-pos 1
                                               :text "This comment isn't submitted yet"))
         (pending-review (make-magit-gh-review :body "I'm still working on this review"
                                               :commit-sha "ghijkl"
                                               :comments (list saved-comment))))
    (magit-gh--store-review-draft pending-review magit-gh--test-pr)
    (should (equal pending-review
                   (magit-gh--get-review-draft magit-gh--test-pr)))))


(defun magit-gh--simulate-adding-comments (comments)
  (dolist (comment comments)
    (with-mocks ((magit-gh--get-current-pr (lambda () magit-gh--test-pr))
                 (magit-gh--cur-github-diff-pos (lambda () (magit-gh-comment-gh-pos comment)))
                 (magit-current-file (lambda () (magit-gh-comment-file comment)))
                 (magit-diff--dwim (lambda () (magit-gh-pr-diff-range magit-gh--test-pr))))
      (magit-gh-add-comment nil (magit-gh-comment-text comment)))))

(defun magit-gh--simulate-adding-review-body (text)
  (if-let ((review-body-section (car (magit-gh--filter-sections
                                      (lambda (sec)
                                        (equal 'review-body (oref sec type)))))))
      (progn
        (magit-section-goto review-body-section)
        (execute-kbd-macro (kbd "C-c '"))
        (insert text)
        (execute-kbd-macro (kbd "C-c '")))
    (error "Couldn't find a magit-section corresponding to the review body")))

(setq magit-gh--test-comments
      (let ((comment1 (make-magit-gh-comment :file "f"
                                             :gh-pos 1
                                             :commit-sha "ghijkl"
                                             :text "Comment 1"))
            (comment2 (make-magit-gh-comment :file "f"
                                             :gh-pos 2
                                             :commit-sha "ghijkl"
                                             :text "Comment 2")))
        (list comment1 comment2)))

(ert-deftest magit-gh--test-add-pending-comments ()
  (magit-gh--discard-review-draft magit-gh--test-pr)
  (magit-gh--simulate-adding-comments magit-gh--test-comments)
  (should (equal (magit-gh-review-comments (magit-gh--get-review-draft magit-gh--test-pr))
                 (reverse magit-gh--test-comments))))


(ert-deftest magit-gh--test-submit-pending-comments-with-body ()
  (magit-gh--discard-review-draft magit-gh--test-pr)
  (let (mock-calls)
    (with-mocks ((magit-gh--request-sync-internal #'mock-github-api)
                 (magit-gh--get-current-pr (lambda () magit-gh--test-pr))
                 (magit-gh--post-review (lambda (&rest args) (push args mock-calls))))
      (magit-gh--simulate-adding-comments magit-gh--test-comments)
      (magit-gh-start-review)
      (magit-gh--simulate-adding-review-body "Super-great job")
      (magit-gh-submit-review)
      (should (equal (list magit-gh--test-pr
                           (make-magit-gh-review :body "Super-great job"
                                                 :comments (reverse magit-gh--test-comments)
                                                 :commit-sha "ghijkl"
                                                 :state 'comment))
                     (car mock-calls))))))

(ert-deftest magit-gh--test-submit-pending-comments-without-body ()
  (magit-gh--discard-review-draft magit-gh--test-pr)
  (let (mock-calls)
    (with-mocks ((magit-gh--request-sync-internal #'mock-github-api)
                 (magit-gh--get-current-pr (lambda () magit-gh--test-pr))
                 (magit-gh--post-pr-comment (lambda (&rest args) (push args mock-calls))))
      (magit-gh--simulate-adding-comments magit-gh--test-comments)
      (magit-gh-start-review)
      (magit-gh-submit-review)
      (should (= (length mock-calls)
                 (length magit-gh--test-comments)))
      (-zip-with (lambda (kall comment)
                   (-let [(pr filename commit-id gh-pos comment-text) kall]
                     (should (equal gh-pos (magit-gh-comment-gh-pos comment)))
                     (should (equal comment-text (magit-gh-comment-text comment)))
                     (should (equal filename (magit-gh-comment-file comment)))
                     (should (equal commit-id (magit-gh-comment-commit-sha comment)))))
                 mock-calls
                 magit-gh--test-comments))))

(ert-deftest magit-gh--test-review-buffer-persistence ()
  (with-mocks ((magit-gh--request-sync-internal #'mock-github-api)
               (magit-gh--get-current-pr (lambda () magit-gh--test-pr)))
    (magit-gh-show-reviews magit-gh--test-pr)
    (let ((review-buf (current-buffer)))
      (should (string= (buffer-name review-buf) "PR: magit-gh-comments (#0)"))
      (magit-gh-show-reviews magit-gh--test-pr)
      (should (equal review-buf (current-buffer))))))

(ert-deftest magit-gh--test-submission-rejected-if-empty ()
  (magit-gh--discard-review-draft magit-gh--test-pr)
  (with-mocks ((magit-gh--request-sync-internal #'mock-github-api)
               (magit-gh--get-current-pr (lambda () magit-gh--test-pr)))
    (magit-gh-start-review)
    (should-error (magit-gh-submit-review) :type 'user-error)))

(ert-deftest magit-gh--test-submission-rejected-if-review-not-started ()
  (should-error (magit-gh-submit-review) :type 'user-error))

(ert-deftest magit-gh--test-submission-review-body-only ()
  (magit-gh--discard-review-draft magit-gh--test-pr)
  (let (mock-calls)
    (with-mocks ((magit-gh--request-sync-internal #'mock-github-api)
                 (magit-gh--get-current-pr (lambda () magit-gh--test-pr))
                 (magit-gh--post-review (lambda (&rest args) (push args mock-calls))))
      (magit-gh-start-review)
      (magit-gh--simulate-adding-review-body "Super-great job")
      (magit-gh-submit-review)
      (should (equal (list magit-gh--test-pr
                           (make-magit-gh-review :body "Super-great job"
                                                 :comments nil
                                                 :commit-sha "ghijkl"
                                                 :state 'comment))
                     (car mock-calls))))))
