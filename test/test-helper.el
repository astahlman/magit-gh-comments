;;; test-helper.el --- Helpers for magit-gh-comments-test.el

(require 'magit)


(defun magit-gh--gen-random-string (str-len)
  (let ((gen-random-char (lambda (n) (+ ?a (random 26)))))
    (apply #'string (mapcar gen-random-char (number-sequence 1 str-len)))))

(defun magit-gh--gen-random-file (&optional num-lines)
  (with-temp-buffer
    (dotimes (i (or num-lines 50))
      (insert (format "%s) %s\n" (+ 1 i) (magit-gh--gen-random-string 8))))
    (buffer-substring (point-min) (1- (point-max)))))


(defun magit-gh--mutate-file (contents &optional mutation-rate)
  (with-temp-buffer
    (let ((i 0))
      (dolist (line (split-string contents "\n"))
        (setq i (1+ i))
        (let ((maybe-mutated-line (if (<= (random 100) (* 100 (or mutation-rate .2)))
                                      (pcase (random 2)
                                        (0 nil) ;; delete it
                                        (1 (format "%s) %s" i (magit-gh--gen-random-string 8)))) ;; mutate it
                                    line)))
          (when maybe-mutated-line
            (insert maybe-mutated-line "\n")))))
    (buffer-substring (point-min) (1- (point-max)))))


(defun magit-gh--generate-revisions ()
  "Make a file with random contents, then mutate it"
  (let* ((num-lines 50)
         (mutation-rate .2)
         (rev-a (magit-gh--gen-random-file num-lines))
         (rev-b (magit-gh--mutate-file rev-a mutation-rate)))
    (with-temp-file "/tmp/a.txt" (insert rev-a))
    (with-temp-file "/tmp/b.txt" (insert rev-b))
    '("/tmp/a.txt" . "/tmp/b.txt")))


(defun magit-gh--generate-github-diff ()
  (let ((buf-name "*magit-gh-github-diff*")
        (num-context-lines (number-to-string (+ 3 (random 5)))))
    (when (get-buffer buf-name)
      (kill-buffer buf-name))
    (call-process "git" nil buf-name nil "--no-pager" "diff" (format "-U%s" num-context-lines) "--no-index" "/tmp/a.txt" "/tmp/b.txt")
    buf-name))


(defun magit-gh--generate-magit-diff (rev-a rev-b)
  (let ((buf-name "*magit-gh-test*"))
    (when (get-buffer buf-name)
      (kill-buffer buf-name))
    (with-current-buffer (get-buffer-create buf-name)
      (magit-git-wash #'magit-diff-wash-diffs
        "diff" "--no-index" "-p" "--no-prefix"
        rev-a
        rev-b))
    buf-name))

(defun magit-gh--pick-random-diff-pos (diff-buf)
  (let (num-changes)
    (with-current-buffer diff-buf
      (goto-char (point-min))
      (re-search-forward "@@ -[0-9]+,[0-9]+ \\+[0-9]+,[0-9]+ @@")
      (setq num-changes (how-many "^[-\\+]"))
      (re-search-forward "^[-\\+]\\(.+\\)" nil nil (1+ (random num-changes)))
      `((:line-contents . ,(match-string 1))
        (:diff-pos . ,(magit-gh--cur-diff-pos))))))

(defun magit-gh--line-contents-at-github-pos (n diff-buf)
  (with-current-buffer diff-buf
    (goto-char (point-min))
    (while (and (not (magit-gh--try-parse-hunk-header))
                (= 0 (forward-line))))
    (forward-line n)
    (buffer-substring (1+ (point-at-bol)) (point-at-eol))))


;;; test-helper.el ends here