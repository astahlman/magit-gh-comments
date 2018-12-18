;; -*- lexical-binding: t -*-

(require 'magit-gh-comments-utils)

(define-minor-mode magit-gh-prose-mode
  "Minor mode for editing long-form prose in magit-gh

This minor mode is turned on when editing the body of a review or a comment."
  :lighter " magit-gh-prose"
  :keymap (let ((keymap (make-sparse-keymap)))
            (define-key keymap (kbd "C-c '") 'magit-gh-prose-save-and-exit)
            (define-key keymap (kbd "C-g") 'magit-gh-prose-abort)
            keymap))

(defun magit-gh-prose-return-to-source-buffer ()
  (unless magit-gh-prose-source-buffer
    (error "[magit-gh-comments] We lost the source buffer! This is probably a bug"))
  (let ((edit-buffer (current-buffer)))
    (switch-to-buffer-other-window magit-gh-prose-source-buffer)
    (delete-other-windows)
    (kill-buffer edit-buffer)))

(defun magit-gh-prose-replace-section-content (section content)
  (save-excursion
    (with-current-buffer magit-gh-prose-source-buffer
      (let* ((start (marker-position (or (oref section content)
                                         (oref section start))))
             (old-len (- (oref section end) start))
             (new-len (length content)))
        (goto-char start)
        ;; TODO: We shouldn't set this keymap in multiple places
        (let ((text (magit-gh--text-with-keymap content
                                              `(,(kbd "C-c '") . ,'magit-gh--edit-prose))))
          (insert (propertize text 'magit-section section)))
        (insert "\n")
        (delete-region (point)
                       (+ (point) old-len))
        (setf (oref section start) (progn (goto-char start)
                                          (point-marker)))
        (setf (oref section end) (progn (goto-char (+ start new-len))
                                        (point-marker)))))))

(defun magit-gh-prose-save-and-exit ()
  (interactive)
  (let ((edit-buffer (current-buffer))
        (section magit-gh-prose-section-being-edited)
        (content (buffer-substring (point-min) (point-max))))
    (when (not (string-empty-p content))
      (setf (oref section value) content)
      (let ((inhibit-read-only t))
        (magit-gh-prose-replace-section-content section content))
      (setq-local magit-gh-prose-ready-for-exit t)
      (magit-gh-prose-return-to-source-buffer))))

(defun magit-gh-prose-abort ()
  (interactive)
  (setq-local magit-gh-prose-ready-for-exit t)
  (magit-gh-prose-return-to-source-buffer))

(defun magit-gh--confirm-kill-edit-prose-buf ()
  (if (and (bound-and-true-p magit-gh-prose-section-being-edited)
           (buffer-modified-p)
           (not (bound-and-true-p magit-gh-prose-ready-for-exit)))
      (yes-or-no-p "Discard unsaved changes? [y/n]")
    t))

(defun magit-gh--init-prose-editor (section source-buffer)
  (magit-gh-prose-mode 1)
  (setq-local magit-gh-prose-section-being-edited
              section)
  (setq-local magit-gh-prose-source-buffer
              source-buffer)
  (setq-local header-line-format
              (substitute-command-keys
               "Edit, then save and exit with `\\[magit-gh-prose-save-and-exit]' or abort and discard changes with \
`\\[magit-gh-prose-abort]'"))
  (when (not (memq 'magit-gh--confirm-kill-edit-prose-buf kill-buffer-query-functions))
    (add-to-list 'kill-buffer-query-functions 'magit-gh--confirm-kill-edit-prose-buf))
  (when-let ((initial-text (oref section value)))
    (insert initial-text)))

(defun magit-gh--edit-prose ()
  (interactive)
  (let* ((section (magit-current-section))
         (source-buffer (current-buffer))
         (buf-name (format "magit-gh: %s" (oref section type)))
         (buf (get-buffer-create buf-name))
         (keymap (make-sparse-keymap)))
    (when (not section)
      (user-error "Point must be on a magit-section before editing"))
    (delete-other-windows)
    (switch-to-buffer-other-window buf)
    (magit-gh--init-prose-editor section source-buffer)))

(provide 'magit-gh-comments-prose)

;;; magit-gh-comments-prose.el ends here
