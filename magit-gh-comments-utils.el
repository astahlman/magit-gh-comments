;; -*- lexical-binding: t -*-

(defun magit-gh--pretty-print (x)
  "Pretty print the form X in a buffer named *pretty*"
  (when (get-buffer "*pretty*")
    (kill-buffer "*pretty*"))
  (with-current-buffer (get-buffer-create "*pretty*")
    (cl-prettyprint x))
  (view-buffer-other-window "*pretty*"))

(defun magit-gh--comment-at-point ()
  "Return the comment overlay at point, if it exists."
  (car (-filter (lambda (ov) (and (overlay-get ov 'magit-gh-comment)
                                  (>= (overlay-start ov) (point))
                                  (<= (overlay-end ov) (point))))
                (-flatten (overlay-lists)))))

(defun magit-gh--overlays-at-point ()
  "Return overlays which touch point.

The start and end of the overlays are inclusive."
  (interactive)
  (filter (lambda (ov) (and (>= (point) (overlay-start ov))
                            (<= (point) (overlay-end ov))))
          (-flatten (overlay-lists))))

(defun magit-gh--delete-overlays-at-point ()
  (interactive)
  (dolist (ov (magit-gh--overlays-at-point))
    (delete-overlay ov)))

;; TODO: This probably doesn't belong here
(defun magit-gh--text-with-keymap (s &rest bindings)
  "Propertize string S with BINDINGS added to 'keymap.

Each element of BINDINGS is a dotted pair of a (kbd) keycode and
a function"
  (let ((keys (make-sparse-keymap)))
    (dolist (binding bindings)
      (define-key keys (car binding) (cdr binding)))
    (propertize s 'keymap keys)))

(defun magit-gh--map-tree (fn root)
  (when root
    (cons (funcall fn root)
          (mapcar (lambda (child) (map-tree fn child)) (oref root children)))))
;; e.g.,
;; (magit-gh--map-tree (lambda (root) (oref root type)) fake-magit-root-section)

(require 'cl-extra)

(defun ht-print (_ht)
  "Return the string representation of the given hashtable"
  (interactive)
  (cl-flet ((pprint (x)
                    (with-temp-buffer
                      (cl-prettyprint x)
                      (buffer-substring (1+ (point-min))  ;; drop leading \n
                                        (point-max)))))
    (with-temp-buffer
      (ht-aeach (insert (format "\t%s => %s\n" key (pprint value)))
                _ht)
      (buffer-string))))

(provide 'magit-gh-comments-utils)
