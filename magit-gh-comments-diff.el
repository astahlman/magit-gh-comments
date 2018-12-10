;; -*- lexical-binding: t; coding: utf-8 -*-

(defconst magit-gh--hunk-header-re "@@ -\\([0-9]+\\),\\([0-9]+\\) \\+\\([0-9]+\\),\\([0-9]+\\) @@")

(defun magit-gh--jump-to-file-in-diff (file)
  (if-let ((result (or (re-search-forward (format "^\\(copy\\|rename\\) to %s$" file) nil t)
                       (re-search-forward (format "+++ b/%s" file) nil t))))
      result
    (error "Could not find file '%s' in diff!" file)))

(defun magit-gh--paint-diff ()
  (while (re-search-forward magit-gh--hunk-header-re nil t)
    (while (not (eobp))
      (put-text-property (point) (min (point-max) (1+ (line-end-position))) 'face
                         (cond
                          ((looking-at-p "^\\+") 'magit-diff-added)
                          ((looking-at "^-") 'magit-diff-removed)
                          (t 'magit-diff-context)))
      (forward-line 1))))


(defun magit-gh--find-by-face (face)
  "Search forward for the next occurrence of text with FACE.

If a match is found, point is placed at the beginning of the
match and the match's length is returned."
  (interactive "Sface")
  (let ((original-point (point))
        stop)
    (while (not (or stop
                    (equal face (get-text-property (point) 'face))))
      (if-let ((beg (next-single-property-change (point) 'face)))
          (goto-char beg)
        (setq stop t)))
    (if (not stop)
        (- (next-single-property-change (point) 'face) (point))
      (and (goto-char original-point) nil))))


(defun magit-gh--wash-diffstat (&rest _args)
  (save-excursion
    (magit-diff-wash-diffstat))
  (let (match)
    (while (setq match-len (magit-gh--find-by-face 'magit-filename))
      (let* ((start (point))
             (end (+ (point) match-len))
             (filepath (buffer-substring-no-properties start end))
             (diff-range (magit-gh-pr-diff-range (car magit-refresh-args))))
        (put-text-property start end
                           'face 'button)
        (let ((keymap (make-sparse-keymap)))
          (define-key keymap (kbd "<return>")
            (let ((filepath filepath)
                  (diff-range diff-range)
                  (pr (magit-gh--get-current-pr)))
              (lambda ()
                (interactive)
                (magit-diff diff-range)
                (setq-local magit-gh--current-pr pr)
                (if-let ((section (magit-get-section
                                   `((file . ,filepath) (diffbuf)))))
                    (progn
                      (goto-char (oref section start))
                      (recenter 0))
                  (error "Couldn't find a file in the diff named `%s'" filepath)))))
          (put-text-property start end 'keymap keymap))
        (goto-char end))))
  (goto-char (point-max)))

(provide 'magit-gh-comments-diff)
