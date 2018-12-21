;;; magit-review.el --- Comment on Github Pull Requests via Magit  -*- lexical-binding: t; coding: utf-8 -*-

(require 'magit-gh-comments-core)

(defvar magit-review-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-mode-map)
    (define-key map "C-c C-c" 'magit-gh-submit-review)
    map)
  "Keymap for `magit-review-mode'.")

(define-derived-mode magit-review-mode magit-mode "Magit Review"
  "Mode for reviewing a Github Pull Request."
  :group 'magit-review)

;; TODO: Make nice customizable faces
(defgroup magit-review nil
  "Review Github Pull Requests from magit"
  :group 'code)

(defun magit-gh-review--lock-value (pr &rest _args)
  "Use the PR as the unique identifier for this magit buffer.

See also `magit-buffer-lock-functions'."
  pr)

(push (cons 'magit-review-mode #'magit-gh-review--lock-value)
      magit-buffer-lock-functions)

(defun magit-review-refresh-buffer (pr &rest _refresh-args)
  (setq-local magit-gh--current-pr pr)
  (magit-gh--populate-pending-review pr))

(defun magit-gh--populate-pending-review (pr)
  (let* ((pr (magit-gh--get-current-pr))
         (review (or (magit-gh--get-review-draft pr)
                     (make-magit-gh-review :state 'pending
                                           :author "you")))
         (diff-body (magit-gh--fetch-diff-from-github pr)))
    (magit-gh--insert-reviews pr (list review) diff-body))
  (goto-char (point-min)))

(defun magit-gh-submit-review ()
  (interactive)
  (let* ((pr (magit-gh--get-current-pr))
         (review (or (magit-gh--get-review-draft pr)
                     (make-magit-gh-review :state 'pending
                                           :commit-sha (cdr (magit-split-range
                                                             (magit-gh-pr-diff-range pr))))))
         (comments (and review (magit-gh-review-comments review)))
         (review-body-section (car (magit-gh--filter-sections
                                    (lambda (section)
                                      (equal (oref section type)
                                             'review-body)))))
         (body (magit-gh--get-review-body review-body-section)))
    (when body
      (setf (magit-gh-review-body review) body))
    (when (not (or comments body))
      (user-error "There is no pending review for %s - please add a comment before submitting."
                  (magit-gh-pr-to-string pr)))
    (magit-gh--post-review pr review)))

(provide 'magit-review)
