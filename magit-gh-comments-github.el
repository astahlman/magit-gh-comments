;; -*- lexical-binding: t -*-

(require 'cl)
(require 'json)
(require 'request)
(require 'magit-git)

;; https://developer.github.com/v3/pulls/#list-pull-requests
;; TODO: Add title, body, and state (at a minimum)
(defstruct magit-gh-pr
  owner
  repo-name
  pr-number
  diff-range
  title
  state
  body)

(setq request-message-level 'debug)
(setq request-log-level 'debug)

(defvar magit-gh--request-timeout-seconds 5)

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

(defun magit-gh--url-for-pr-reviews (pr)
  (format "%s/reviews" (magit-gh--url-for-pr pr)))

;; TODO: Put this back, just don't want to wipe out my cache without wifi
(setq magit-gh--request-cache nil)
(cl-defun magit-gh--request-sync (url &rest request-args)
   (let ((response (or (cdr (assoc (list url request-args) magit-gh--request-cache))
                         (let ((http-result (apply #'magit-gh--request-sync-internal url request-args)))
                           (map-put magit-gh--request-cache
                                    (list url request-args)
                                    http-result)
                           http-result))))
       (if (listp response)
           (magit-gh--remove-carriage-returns
            (magit-gh--keys->keywords response))
         response)))

(defun magit-gh--remove-carriage-returns (l)
  "Recursively remove ^M from any strings in list L"
  (cond
   ((not l) nil)
   ((stringp l) (s-replace "" "" l))
   ((consp l) (cons (magit-gh--remove-carriage-returns (car l))
                    (magit-gh--remove-carriage-returns (cdr l))))
   (t l)))

(ert-deftest test-magit-gh--remove-carriage-returns ()
  (should (equal (magit-gh--remove-carriage-returns "foo\nbar")
                 "foo\nbar"))
  (should (equal (magit-gh--remove-carriage-returns '((:a . 1) (:b . "foo\nbar")))
                 '((:a . 1) (:b . "foo\nbar"))))
  (should (equal (magit-gh--remove-carriage-returns '(2 ("ab" . "foo\nbar")))
                 '(2 ("ab" . "foo\nbar"))))
  (should (equal (magit-gh--remove-carriage-returns "foobar") "foobar"))
  (should (not (equal (magit-gh--remove-carriage-returns "foobar") "foobar"))))


(cl-defun magit-gh--request-sync-internal (url &rest request-args
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
  (lexical-let (result err-code finished-p)
    (apply #'request url
           :success (function* (lambda (&key data &allow-other-keys)
                                 (setq result data
                                       finished-p t)))
           :parser parser
           :timeout timeout
           :error (function* (lambda (&key response data &allow-other-keys)
                               (setq result data
                                     finished-p t
                                     err-code (request-response-status-code response))))
           request-args)
    (with-timeout (magit-gh--request-timeout-seconds
                   (error "Timeout in request to Github!"))
      ;; Ensure the callbacks have had a chance to run before returning
      (while (not finished-p)
        (sit-for .05))
      (if err-code
          (error "Error fetching from %s [%s]: %s" url err-code result)
        result))))

(defun magit-gh--extract-header (key request-args)
  (cdr (assoc key (plist-get request-args :headers))))

(defun magit-gh--hydrate-pr-from-github (pr)
  "Hydrate PR with additional fields fetched from Github.

This function modifies and returns its input."
  (let ((response (magit-gh--request-sync (magit-gh--url-for-pr pr)
                                          :headers `(("Authorization" . ,(format "token %s" (magit-gh--get-oauth-token))))
                                          :parser #'json-read)))
    (setf (magit-gh-pr-body pr) (alist-get :body response))
    (setf (magit-gh-pr-title pr) (alist-get :title response))
    (setf (magit-gh-pr-state pr) (alist-get :state response))
    pr))

(defun magit-gh--fetch-diff-from-github (pr)
  (magit-gh--request-sync (magit-gh--url-for-pr pr)
                          :headers `(("Authorization" . ,(format "token %s" (magit-gh--get-oauth-token)))
                                     ("Accept" . "application/vnd.github.v3.diff"))))

(defun magit-gh--fetch-diff-for-commit-from-github (sha)
  (magit-gh--request-sync (format "https://api.github.com/repos/astahlman/magit-gh-comments/commits/%s" sha)
                          :headers `(("Authorization" . ,(format "token %s" (magit-gh--get-oauth-token)))
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
  (apply 'append
         (mapcar (lambda (review) (alist-get :comments review))
                 (magit-gh--list-reviews pr))))


;; TODO: Make an integration test out of this:
;; (magit-gh--list-comments magit-gh-comment-test-pr)
;; (magit-gh--list-reviews magit-gh-comment-test-pr)

(defun magit-gh--assoc-recursive (keys alist)
  (cond ((not (listp keys)) (magit-gh--assoc-recursive (list keys) alist))
        ((not keys) nil)
        ((not (listp alist)) nil)
        ((not (cdr keys)) (assoc (car keys) alist))
        (t (magit-gh--assoc-recursive (cdr keys) (cdr (assoc (car keys) alist))))))


(defun magit-gh--parse-json-array ()
 (let ((json-array-type 'list))
   (json-read-array)))

(defun magit-gh--keys->keywords (l)
  "Recursively transform the keys of the alists in L from strings or keywords
to colon-prefixed keywords. L can be an alist or a list of alists."
  (letfn ((simple-alistp (lambda (xs)
                           (and (consp xs)
                                (ignore-errors (-every? #'consp xs))
                                (-every? (lambda (pair) (or (symbolp (car pair))
                                                            (stringp (car pair)))) xs))))
          (transform-dotted-pair (lambda (dp)
                             (cons
                              (intern (format ":%s" (car dp)))
                              (magit-gh--keys->keywords (cdr dp))))))
    (cond ((not l) nil)
          ((atom l) l)
          ((simple-alistp l) (mapcar 'transform-dotted-pair l))
          (t (cons (magit-gh--keys->keywords (car l))
                   (magit-gh--keys->keywords (cdr l)))))))

(ert-deftest test-magit-gh--keys->keywords ()
  ;; Simple alist
  (should (equal (magit-gh--keys->keywords '(("a" . 1) ("b". 2)))
                 '((:a . 1) (:b . 2))))
  ;; List of alists
  (should (equal (magit-gh--keys->keywords '((("a" . 1) ("b" . (("a1" . 2) ("b2" . (3 4)))))))
                 '(((:a . 1) (:b . ((:a1 . 2) (:b2 . (3 4))))))))
  ;; Nested alists
  (should (equal (magit-gh--keys->keywords '(("a" . 1) ("b" . (("c" . 3)))))
                 '((:a . 1) (:b . ((:c . 3))))))
  ;; List of alists with nested alists
  (should (equal (magit-gh--keys->keywords '((("a" . 1) ("b" . (("c" . 3))))))
                 '(((:a . 1) (:b . ((:c . 3))))))))

(defun magit-gh--alist-filter (keys alist)
  (-filter 'identity ;; remove nils
          (mapcar (lambda (k) (magit-gh--assoc-recursive k alist))
                  keys)))

(defun magit-gh--rename-key (alist from to)
  "Replace all keys in ALIST where value is FROM with the value TO."
  (mapcar (lambda (pair)
            (if (equal from (car pair))
                `(,to . ,(cdr pair))
              pair))
          alist))

(defun magit-gh--alist-values (alist)
  (mapcar #'cdr alist))

(defun magit-gh--list-reviews (pr)
  "Return a list of reviews on the given PR."
  (let* ((reviews (magit-gh--request-sync
                   (magit-gh--url-for-pr-reviews pr)
                   :headers `(("Authorization" . ,(format "token %s" (magit-gh--get-oauth-token))))
                   :parser #'magit-gh--parse-json-array))
         (reviews (mapcar (lambda (review)
                            (magit-gh--alist-filter '(:id (:user :login) :body :state)
                                                    review))
                          reviews))
         (reviews (mapcar (lambda (review) (magit-gh--rename-key review :login :author)) reviews))
         (comments (magit-gh--request-sync
                    (magit-gh--url-for-pr-comments pr)
                    :headers `(("Authorization" . ,(format "token %s" (magit-gh--get-oauth-token))))
                    :parser #'magit-gh--parse-json-array))
         (comments (mapcar (lambda (comment)
                             (magit-gh--alist-filter '(:pull_request_review_id (:user :login) :body :path :position :original_position :original_commit_id)
                                                     comment))
                           comments))
         (comments (mapcar (lambda (comment) (magit-gh--rename-key comment :login :author)) comments))
         (comments (mapcar (lambda (comment)
                             (add-to-list 'comment `(:is_outdated . ,(not (alist-get :position comment)))))
                           comments)))
    ;; review-id -> ((:comments ...) (:id . $id) (:body . $body) (:author . $user))
    (let* ((reviews (-group-by (-partial #'alist-get :id) reviews))
           ;; alist values are list of lists - unnest them as a single list
           (reviews (mapcar (lambda (x) `(,(car x) . ,(cadr x))) reviews)))
      (dolist (comment comments (magit-gh--alist-values reviews))
        (let* ((review-id (or (alist-get :pull_request_review_id comment)
                              (error "Could not find the review associated with this comment!")))
               (review (alist-get review-id reviews))
               (review-comments (alist-get :comments review)))
          (map-put review :comments
                   (cons comment review-comments))
          (map-put reviews review-id review))))))

;; (setq my-reviews (magit-gh--list-reviews magit-gh-comment-test-pr))
;; (magit-gh--pretty-print my-reviews)

(provide 'magit-gh-comments-github)
