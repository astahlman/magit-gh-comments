;; -*- lexical-binding: t -*-

(require 'cl)
(require 'json)
(require 'request)
(require 'magit-git)

(defconst MAGIT-GH-DEBUG t)
(defstruct magit-gh-pr owner repo-name pr-number)

(setq request-message-level 'debug)
(setq request-log-level 'debug)

(setq magit-gh-comment-test-pr (make-magit-gh-pr :owner "astahlman"
                                                 :repo-name "magit-gh-comments"
                                                 :pr-number 3))

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

(defun magit-gh--fetch-diff-from-github (pr)
  (if MAGIT-GH-DEBUG
      magit-gh--cached-diff
      (let ((result))
        (request
         (magit-gh--url-for-pr pr)
         :headers '(("Authorization" . (format "token %s" (magit-gh--get-oauth-token)))
                    ("Accept" . "application/vnd.github.v3.diff"))
         :parser (lambda ()
                   (setq result (buffer-string)))
         :sync t
         :error (function*
                 (lambda (&key error-thrown &allow-other-keys)
                   (error "Failed to fetch diff from Github: %s" error-thrown))))
        (setq magit-gh--cached-diff result)
        result)))

(defun magit-gh--comment-as-json (filename commit-sha gh-pos comment-text)
  (json-encode `((:body . ,comment-text)
                 (:commit_id . ,commit-sha)
                 (:path . ,filename)
                 (:position . ,gh-pos))))

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
  "Return a list of comments that aren't outdated for the given
PR. A comment is outdated if its position is null, according to
Github. Each element of the result is an alist with the following keys:

:body - The text of the comment
:position - The comment's Github-style position in the diff
:author - The Github username of the comments' author
:path - The path to the file to which this comment applies"
  (let ((comment-fields '(body position author path))
        (url (magit-gh--url-for-pr-comments pr))
        (alist-filter (lambda (keys alist)
                        (filter 'identity ;; remove nils
                                (mapcar (lambda (k)
                                          (assoc k alist))
                                        keys))))
        (outdated-p (lambda (comment) (not (alist-get 'position comment))))
        (alist-map-keys (lambda (fn alist)
                          (let ((result))
                            (reverse
                             (dolist (cell alist result)
                               (setq result
                                     (cons
                                      (cons (funcall fn (car cell))
                                            (cdr cell))
                                      result)))))))
        (sym-to-keyword (lambda (x) (intern (format ":%s" x))))
        (result))
    (progn
      (if MAGIT-GH-DEBUG
          (setq result magit-gh--cached-comments)
        (request
         url
         :headers '(("Authorization" . (format "token %s" (magit-gh--get-oauth-token))))
         :parser (lambda ()
                   (let ((json-array-type 'list))
                     (json-read-array)))
         :sync t
         :success) (cl-function
                    (lambda (&key data &allow-other-keys)
                      (dolist (comment (remove-if outdated-p data))
                        (setq result
                              (cons (funcall alist-filter
                                             comment-fields
                                             comment)
                                    result))
                        (setq magit-gh--cached-comments result)))))
      (mapcar (lambda (l) (funcall alist-map-keys sym-to-keyword l))
              (reverse result)))))

(provide 'magit-gh-comments-github)
