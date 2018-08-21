;; -*- lexical-binding: t -*-

(require 'ert)
(require 'magit-gh-comments)

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

(ert "test-magit-gh--.*")
