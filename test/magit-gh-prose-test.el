(require 'cl)
(require 'subr-x)
(require 'magit-gh-comments-prose)

(defmacro with-point-on-magit-section (content &rest body)
  (declare (indent 1) (debug t))
  (let ((section (gensym)))
    `(let (,section)
       (magit-insert-section (test)
         (magit-insert-heading "Fake Review Body Header")
         (insert ,content))
       (magit-section-backward)
       (setq ,section (magit-current-section))
       (setf (oref ,section value) ,content)
       ,@body
       ,section)))

(ert-deftest magit-gh-test-populate-empty-section ()
  (magit-gh--with-temp-buffer
    (let ((section (with-point-on-magit-section ""
                     (magit-gh--edit-prose))))
      (insert "foo")
      ;; save-and-exit
      (execute-kbd-macro (kbd "C-c '"))
      (should (string= (oref section value)
                       "foo"))
      (should (string= (oref section value)
                       (magit-gh--section-content-as-string section))))))

(ert-deftest magit-gh-test-edit-save-and-exit ()
  (magit-gh--with-temp-buffer
    (let ((section (with-point-on-magit-section "foo"
                     (magit-gh--edit-prose))))
      (insert " bar")
      ;; save-and-exit
      (execute-kbd-macro (kbd "C-c '"))
      (should (string= (oref section value)
                       "foo bar"))
      (should (string= (oref section value)
                       (magit-gh--section-content-as-string section))))))

(ert-deftest magit-gh-test-edit-and-abort ()
  (magit-gh--with-temp-buffer
    (let ((section (with-point-on-magit-section "foo"
                     (magit-gh--edit-prose))))
      (insert "None of this will be saved")
      ;; abort
      (execute-kbd-macro (kbd "C-g"))
      (should (string= (oref section value)
                       "foo"))
      (should (string= (oref section value)
                       (magit-gh--section-content-as-string section))))))
