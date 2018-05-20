; -*- lexical-binding: t -*-

(require 'request)
(require 'magit-git)

(defstruct magit-gh-pr owner repo-name pr-number)

(defun magit-gh--get-oauth-token ()
  (let ((token (magit-get "github" "oauth-token")))
    (assert (not (string-empty-p token)) t
            "Couldn't find a valid token. Is [github].oauth-token set in your .gitconfig?")
    token))

(defun magit-gh--url-for-pr (pr)
  (format "https://api.github.com/repos/%s/%s/pulls/%s"
          (magit-gh-pr-owner pr)
          (magit-gh-pr-repo-name pr)
          (magit-gh-pr-pr-number pr)))

(defun magit-gh--url-for-pr-comments (pr)
  (format "%s/comments" (magit-gh--url-for-pr pr)))

(defun magit-gh--fetch-diff-from-github (pr target-buf)
  (request
   (magit-gh--url-for-pr pr)
   :headers '(("Authorization" . (format "token %s" (magit-gh--get-oauth-token)))
              ("Accept" . "application/vnd.github.v3.diff"))
   :parser (lambda ()
             (let ((diff-content (buffer-substring (point-min) (point-max))))
               (with-current-buffer target-buf
                 (goto-char (point-min))
                 (insert diff-content))))
   :error (function*
           (lambda (&key error-thrown &allow-other-keys)
             (error "Failed to fetch diff from Github: %s" error-thrown)))))

(setq magit-gh-comment-test-pr (make-magit-gh-pr :owner "astahlman"
                                                 :repo-name "magit-gh-comments"
                                                 :pr-number 2))

(defun magit-gh--comment-as-json (filename commit-sha gh-pos comment-text)
  (json-encode `((:body . ,comment-text)
                 (:commit_id . ,commit-sha)
                 (:path . ,filename)
                 (:position . ,gh-pos))))

(magit-gh--comment-as-json "test-file.txt" "562b1f07ede9c579ae5ec2d79a07879dd7a0d031" 7 "hi")
(magit-gh--fetch-diff-from-github magit-gh-comment-test-pr (get-buffer-create "pr-2-diffs"))

(defun magit-gh--post-pr-comment (pr filename commit-id gh-pos comment-text)
  (let ((url (magit-gh--url-for-pr-comments pr))
        (json-payload (magit-gh--comment-as-json filename
                                                 commit-id
                                                 gh-pos
                                                 comment-text)))
    (request
     url
     :type "POST"
     :headers `(("Authorization" . ,(format "token %s" (magit-gh--get-oauth-token)))
                ("Content-Type" . "application/json")
                ("Accept" . "application/vnd.github.v3.json"))
     :data json-payload
     :parser (lambda ()
               (write-file "/tmp/response.txt"))
     :complete (function*
             (lambda (&key response &allow-other-keys)
               (if (not (member (request-response-status-code response) '(200 201)))
                   (error "Failed to post comment to %s" url)))))))


(defun magit-gh--list-comments (pr)
  (let ((url (magit-gh--url-for-pr-comments pr)))
    (request
     url
     :headers '(("Authorization" . (format "token %s" (magit-gh--get-oauth-token))))
     :parser (lambda ()
               (write-file "/tmp/response2.txt")))))

(setq request-message-level 'debug)
(setq request-log-level 'debug)

(provide 'magit-gh-comments-github)
