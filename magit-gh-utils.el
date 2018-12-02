(defun magit-gh--pretty-print (x)
  "Pretty print the form X in a buffer named *pretty*"
  (when (get-buffer "*pretty*")
    (kill-buffer "*pretty*"))
  (with-current-buffer (get-buffer-create "*pretty*")
    (cl-prettyprint x))
  (view-buffer-other-window "*pretty*"))

(magit-gh--pretty-print magit-gh--request-cache)

(defun magit-gh--comment-at-point ()
  "Return the comment overlay at point, if it exists."
  (car (-filter (lambda (ov) (and (overlay-get ov 'magit-gh-comment)
                                  (>= (overlay-start ov) (point))
                                  (<= (overlay-end ov) (point))))
                (-flatten (overlay-lists)))))

(defun overlays-at-point ()
  "Return overlays which touch point.

The start and end of the overlays are inclusive."
  (interactive)
  (filter (lambda (ov) (and (>= (point) (overlay-start ov))
                            (<= (point) (overlay-end ov))))
          (-flatten (overlay-lists))))


(defun delete-overlays-at-point ()
  (interactive)
  (dolist (ov (overlays-at-point))
    (delete-overlay ov)))
