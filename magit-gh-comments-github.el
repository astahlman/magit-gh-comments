;; -*- lexical-binding: t -*-

(require 'cl)
(require 'json)
(require 'request)
(require 'magit-git)

(defstruct magit-gh-pr owner repo-name pr-number)

(setq request-message-level 'info)
(setq request-log-level 'info)

(defvar magit-gh--request-timeout-seconds 3)

(setq magit-gh-comment-test-pr (make-magit-gh-pr :owner "astahlman"
                                                 :repo-name "magit-gh-comments"
                                                 :pr-number 2))

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

(cl-defun magit-gh--request-sync (url &rest request-args
                                      &key
                                      (parser #'buffer-string)
                                      (timeout magit-gh--request-timeout-seconds)
                                      &allow-other-keys)
  "Execute an authorized, synchronous HTTP request.

This function is guaranteed to either timeout, error out, or
return the body of the HTTP response.

request.el does support synchronous requests with the argument
`:sync t', but this turns out to have a race condition in which
the function may return before all slots of the
`request-response' object are populated. By calling `sit-for', we
ensure that the process sentinels for the curl command have
completed and our callbacks have set a return value before this
function returns."
  (lexical-let (result err-code)
    (apply #'request url
           :success (function* (lambda (&key data &allow-other-keys)
                                 (setq result data)))
           :parser parser
           :timeout timeout
           :error (function* (lambda (&key response data &allow-other-keys)
                               (setq result data)
                               (setq err-code (request-response-status-code response))))
           request-args)
    (with-timeout (magit-gh--request-timeout-seconds
                   (error "Timeout in request to Github!"))
      ;; Ensure the callbacks have had a chance to run before returning
      (while (and (not result)
                  (not err-code))
        (sit-for .05))
      (if err-code
          (error "Error fetching from %s [%s]: %s" url err-code result)
        result))))

(defun magit-gh--fetch-diff-from-github (pr)
  (magit-gh--request-sync (magit-gh--url-for-pr pr)
                          :headers '(("Authorization" . (format "token %s" (magit-gh--get-oauth-token)))
                                     ("Accept" . "application/vnd.github.v3.diff"))))

;; TODO: Make an integration test out of this:
;; (magit-gh--fetch-diff-from-github magit-gh-comment-test-pr)

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
    (request url
     :type "POST"
     :headers `(("Authorization" . ,(format "token %s" (magit-gh--get-oauth-token)))
                ("Content-Type" . "application/json")
                ("Accept" . "application/vnd.github.v3.json"))
     :data json-payload
     :complete (function*
             (lambda (&key response &allow-other-keys)
               (if (not (member (request-response-status-code response) '(200 201)))
                   (error "Failed to post comment to %s" url)))))))

(defun magit-gh--list-comments (pr)
  "Return a list of comments on the given PR.

The returned list does not include outdated comments. A comment
is outdated if its position is null, according to Github. Each
element of the result is an alist with the following keys:

:body - The text of the comment
:position - The comment's Github-style position in the diff
:author - The Github username of the comments' author
:path - The path to the file to which this comment applies"
  (cl-flet ((outdated-p (comment)
                        (not (alist-get 'position comment)))
            (sym-to-keyword (x)
                            (intern (format ":%s" x)))
            (alist-filter (keys alist)
                          (filter 'identity ;; remove nils
                                  (mapcar (lambda (k)
                                            (assoc k alist))
                                          keys)))
            (alist-map-keys (fn alist)
                            (let (result)
                              (reverse
                               (dolist (cell alist result)
                                 (setq result
                                       (cons
                                        (cons (funcall fn (car cell))
                                              (cdr cell))
                                        result)))))))
    (let ((comment-fields '(body position author path))
          (comments (magit-gh--request-sync
                     (magit-gh--url-for-pr-comments pr)
                     :headers '(("Authorization" . (format "token %s" (magit-gh--get-oauth-token))))
                     :parser (lambda ()
                               (let ((json-array-type 'list))
                                 (json-read-array)))))
          result)
      (dolist (comment (remove-if #'outdated-p comments))
        (let* ((author (alist-get 'login (alist-get 'user comment)))
               (comment (add-to-list 'comment `(author . ,author))))
          (setq result (cons (alist-filter comment-fields comment)
                             result))))
      (reverse (mapcar
                (lambda (x) (alist-map-keys #'sym-to-keyword x))
                result)))))

;; TODO: Make an integration test out of this:
;; (magit-gh--list-comments magit-gh-comment-test-pr)

(provide 'magit-gh-comments-github)
