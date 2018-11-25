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

(provide 'magit-gh-comments-diff)
