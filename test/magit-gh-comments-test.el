;; -*- lexical-binding: t -*-

(require 'ert)
(require 'magit-gh-comments)

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
