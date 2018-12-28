;;; magit-pull-request.el --- Comment on Github Pull Requests via Magit  -*- lexical-binding: t; coding: utf-8 -*-

(require 'magit)
(require 'magit-gh-comments-core)

;;;###autoload
(define-derived-mode magit-pull-request-mode magit-mode "Magit Pull Request"
  "Mode for looking at a Github Pull Request."
  :group 'magit-pull-request)


(defvar magit-pull-request-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-mode-map)
    (define-key map "k" 'magit-gh-add-comment)
    map)
  "Keymap for `magit-pull-request-mode'.")

;; TODO: Make nice customizable faces
(defgroup magit-pull-request nil
  "Add comments to Github Pull Requests from magit"
  :group 'code)

(defun magit-gh-pull-request--lock-value (pr &rest _args)
  "Uses the PR as the unique identifier for this magit buffer.

See also `magit-buffer-lock-functions'."
  (let ((_pr (car pr)))
    (list
     (magit-gh-pr-owner _pr)
     (magit-gh-pr-repo-name _pr)
     (magit-gh-pr-pr-number _pr))))

;;;###autoload
(push (cons 'magit-pull-request-mode #'magit-gh-pull-request--lock-value)
      magit-buffer-lock-functions)

(defun magit-pull-request-reload-from-github ()
  (interactive)
  (let ((magit-gh--should-skip-cache t))
    (magit-refresh-buffer)))

(defun magit-pull-request-refresh-buffer (pr &rest _refresh-args)
  ;; We'll need a reference to the PR in our magit-diff refresh hook
  (setq-local magit-gh--current-pr (magit-gh--hydrate-pr-from-github pr))
  (magit-gh--populate-reviews magit-gh--current-pr))

(defun magit-gh--populate-reviews (pr)
  "Populate and return the magit reviews buffer for the given PR."
  (magit-insert-section (pull-request pr) ;; root
    (magit-insert-section (summary)
      (magit-insert-heading (format "%s (#%s) [%s]"
                                    (magit-gh-pr-title pr)
                                    (magit-gh-pr-pr-number pr)
                                    (upcase (magit-gh-pr-state pr))))
      (insert "\n"
              (or
               (and (not (string-empty-p (magit-gh-pr-body pr))) (magit-gh-pr-body pr))
               (propertize "No description provided" 'face 'italic)) "\n\n"))
    (magit-insert-section (diffstat)
      (magit-insert-heading "Files changed")
      (insert "\n")
      (magit-git-wash #'magit-gh--wash-diffstat "diff" "--stat" (magit-gh-pr-diff-range pr))
      (insert "\n"))
    (let ((reviews (magit-gh--list-reviews pr))
          (diff-body (magit-gh--fetch-diff-from-github pr)))
      (magit-gh--insert-reviews (magit-gh--get-current-pr)
                                reviews
                                diff-body))
    (goto-char (point-min))
    (current-buffer)))

(provide 'magit-pull-request)
