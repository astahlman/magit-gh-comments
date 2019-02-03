;; -*- lexical-binding: t -*-

(require 'cl)
(require 'json)
(require 'request)
(require 'magit-git)

;; https://developer.github.com/v3/pulls/#list-pull-requests
;; TODO: Add title, body, and state (at a minimum)
(defstruct magit-gh-pr
  owner ;; TODO: Clarify that this is the repo owner, not the PR owner. Maybe we should rename...
  repo-name
  pr-number
  diff-range
  title
  state
  body)

;; ID is the unique Github identifier for this review
;; AUTHOR is the Github handle of the user who submitted the review
;; BODY is the top-level review text
;; THREADS is a list of lists of `magit-gh-comment's; each list is a comment thread
;; COMMIT-SHA is the SHA of the git commit to which the review applies
;; STATE is one of "APPROVE", "REQUEST_CHANGES", or "COMMENT". nil means "PENDING"
(defstruct magit-gh-review
  id
  author
  body
  threads
  commit-sha
  state)

(defun magit-gh-pr-to-string (pr)
  (format "%s#%s"
          (magit-gh-pr-repo-name pr)
          (magit-gh-pr-pr-number pr)))

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

(defvar magit-gh--request-cache nil
  "An alist mapping HTTP requests to responses")

(defvar magit-gh--should-skip-cache nil
  "Dynamically bind to `t' to invalidate the cached response (if
  present) force a new fetch from the Github API")

(cl-defun magit-gh--request-sync (url &rest request-args)
  (let ((response (or (and (not magit-gh--should-skip-cache)
                           (cdr (assoc (list url request-args) magit-gh--request-cache)))
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
  "Hydrate PR with additional fields fetched from Github."
  (let ((response (magit-gh--request-sync (magit-gh--url-for-pr pr)
                                          :headers `(("Authorization" . ,(format "token %s" (magit-gh--get-oauth-token))))
                                          :parser #'json-read))
        (result (copy-magit-gh-pr pr)))
    (setf (magit-gh-pr-body result) (alist-get :body response))
    (setf (magit-gh-pr-title result) (alist-get :title response))
    (setf (magit-gh-pr-state result) (alist-get :state response))
    result))

(defun magit-gh--fetch-diff-from-github (pr)
  (magit-gh--request-sync (magit-gh--url-for-pr pr)
                          :headers `(("Authorization" . ,(format "token %s" (magit-gh--get-oauth-token)))
                                     ("Accept" . "application/vnd.github.v3.diff"))))

(defun magit-gh--url-for-commit (pr sha)
  (format "https://api.github.com/repos/%s/%s/commits/%s"
          (magit-gh-pr-owner pr)
          (magit-gh-pr-repo-name pr)
          sha))

(defun magit-gh--fetch-diff-for-commit-from-github (pr sha)
  (magit-gh--request-sync (magit-gh--url-for-commit pr sha)
                          :headers `(("Authorization" . ,(format "token %s" (magit-gh--get-oauth-token)))
                                     ("Accept" . "application/vnd.github.v3.diff"))))


;; TODO: Get rid of this in favor magit-gh-comment--to-github-format
(defun magit-gh--comment-as-json (filename commit-sha gh-pos comment-text in-reply-to)
  (json-encode `((:body . ,comment-text)
                 (:commit_id . ,commit-sha)
                 (:path . ,filename)
                 (:position . ,gh-pos)
                 (:in_reply_to . ,in-reply-to))))

(defun magit-gh--post-pr-comment (pr filename commit-id gh-pos comment-text &optional in-reply-to)
  (let ((url (magit-gh--url-for-pr-comments pr))
        (json-payload (magit-gh--comment-as-json filename
                                                 commit-id
                                                 gh-pos
                                                 comment-text
                                                 in-reply-to)))
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

(defun magit-gh--reply-to-comment (pr in-reply-to comment-text)
  (magit-gh--post-pr-comment pr nil nil nil comment-text in-reply-to))

;; TODO: Make an integration test out of this:
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


(defun magit-gh--ht-map-vals (table fn)
  "Update TABLE by mapping FN over its values.

This function does not modify its input."
  (let ((result (ht-copy table)))
    (ht-each (lambda (k v)
               (ht-set! result
                        k
                        (funcall fn v)))
             table)
    result))

(defun magit-gh--group-comments-by-thread (comments) ;(reviews)
  (let (;(comment->review (ht-create))
        (comment-id->thread (ht-create))
        (num-pending-comments 0))
    (dolist (comment comments)
      (ht-set! comment-id->thread
               (or (magit-gh-comment-id comment)
                   ;; pending comments can't possibly have a reply,
                   ;; so just assign them some placeholder ID
                   (make-symbol (format ":pending-%s" (incf num-pending-comments))))
               (cons comment nil)))
    (ht-each
     (lambda (id thread)
       (-if-let* ((comment (car thread))
                  (upstream-cons (ht-get comment-id->thread
                                         (magit-gh-comment-in-reply-to comment))))
           (setcdr (last upstream-cons) thread)))
     comment-id->thread)
    (--> comment-id->thread
         ;; Discard entries that represent partial threads
         (ht-select (lambda (id thread)
                      (not (magit-gh-comment-in-reply-to (car thread))))
                    it)
         ;; Sort based on timestamp
         (magit-gh--ht-map-vals
          it
          (lambda (thread)
            (let ((comment< (lambda (x y)
                              (if (equal (magit-gh-comment-in-reply-to x)
                                         (magit-gh-comment-in-reply-to y))
                                  (string< (magit-gh-comment-created-at x)
                                           (magit-gh-comment-created-at y))
                                ;; surprisingly, Y < X in the input
                                ;; list, so return nil for a stable
                                ;; sort
                                nil))))
              (magit-gh--sort thread
                              #'identity
                              comment<))))
         (ht-values it))))

;; (ert-deftest magit-gh--test-grouping-pending-comments ()
;;   (let ((review (make-magit-gh-review :state 'pending
;;                                       :comments (list
;;                                                  (make-magit-gh-comment)))))
;;     (magit-gh--group-comments-by-thread (list review))))

(ert-deftest magit-gh--test-grouping-pending-comments ()
  (let ((pending-comment (make-magit-gh-comment)))
    (should (equal (list (list pending-comment))
                   (magit-gh--group-comments-by-thread (list pending-comment))))))

(ert-deftest magit-gh--test-grouping-comments-into-threads ()
  (let* ((comment1 (make-magit-gh-comment :id 1))
         (comment2 (make-magit-gh-comment :id 2))
         (comment3 (make-magit-gh-comment :id 3
                                          :in-reply-to 1
                                          :created-at "2018-01-02T00:00:00Z"))
         (comment4 (make-magit-gh-comment :id 4))
         (comment5 (make-magit-gh-comment :id 5
                                          :in-reply-to 1
                                          :created-at "2018-01-01T00:00:00Z"))
         (comment6 (make-magit-gh-comment :id 6
                                          :in-reply-to 3))
         (result (magit-gh--group-comments-by-thread
                  (list comment1 comment2 comment3 comment4 comment5 comment6))))
    (should (equal result
                   (list (list comment4)
                         (list comment2)
                         (list comment1
                               comment5
                               comment3
                               comment6))))))

(defun magit-gh--list-reviews (pr)
  "Return a list of reviews on the given PR."
  (let* ((reviews (magit-gh--request-sync
                   (magit-gh--url-for-pr-reviews pr)
                   :headers `(("Authorization" . ,(format "token %s" (magit-gh--get-oauth-token))))
                   :parser #'magit-gh--parse-json-array))
         (review-id->review (let ((_ht (ht-create)))
                              (dolist (review reviews _ht)
                                (ht-set! _ht
                                         (alist-get :id review)
                                         (make-magit-gh-review
                                          :id (alist-get :id review)
                                          :author (cdr (magit-gh--assoc-recursive '(:user :login) review))
                                          :body (alist-get :body review)
                                          :state (alist-get :state review))))))
         (comments (magit-gh--request-sync
                    (magit-gh--url-for-pr-comments pr)
                    :headers `(("Authorization" . ,(format "token %s" (magit-gh--get-oauth-token))))
                    :parser #'magit-gh--parse-json-array))
         (comments (mapcar (lambda (comment)
                             (make-magit-gh-comment :id (alist-get :id comment)
                                                    :review-id (alist-get :pull_request_review_id comment)
                                                    :file (alist-get :path comment)
                                                    :commit-sha (alist-get :original_commit_id comment)
                                                    :gh-pos (alist-get :position comment)
                                                    :text (alist-get :body comment)
                                                    :author (cdr (magit-gh--assoc-recursive '(:user :login) comment))
                                                    :created-at (alist-get :created_at comment)
                                                    :original-gh-pos (alist-get :original_position comment)
                                                    :is-outdated (and
                                                                  (not (alist-get :position comment))
                                                                  (not (alist-get :in_reply_to_id comment)))
                                                    :in-reply-to (alist-get :in_reply_to_id comment)))
                           comments)))
    (let ((threads (magit-gh--group-comments-by-thread comments)))
      ;; TODO: Probably need to sort here
      (dolist (thread threads)
        (let* ((review-id (magit-gh-comment-review-id (car thread)))
               (review (or (ht-get review-id->review review-id)
                           ;; Hack: Comments do not have to be
                           ;; associated with a review, so we create one
                           ;; on the fly.

                           ;; TODO: Unit test this - remove the (or) to
                           ;; expose the bug
                           (let ((review (make-magit-gh-review :id review-id
                                                               :author (magit-gh-comment-author comment))))
                             (ht-set! review-id->review
                                      review-id
                                      review)
                             review))))
          (push thread (magit-gh-review-threads review)))))
    (magit-gh--sort (ht-values review-id->review) #'magit-gh-review-id)))

(defun magit-gh-comment--to-github-format (comment &optional is-standalone)
  "Return COMMENT as an alist with the fields required by the Github API

API reference: https://developer.github.com/v3/pulls/reviews/#input"
  (let ((result `((:body . ,(magit-gh-comment-text comment))
                  (:path . ,(magit-gh-comment-file comment))
                  (:position . ,(magit-gh-comment-gh-pos comment)))))
    (when is-standalone
      (push `(:commit_id . ,(magit-gh-comment-commit-sha comment)) result))
    result))

(defun magit-gh--post-review (pr review)
  "Submit the REVIEW for PR to Github.

API reference: https://developer.github.com/v3/pulls/reviews/#example"
  (let ((url (magit-gh--url-for-pr-reviews pr))
        (payload `((:commit_id . ,(magit-gh-review-commit-sha review))
                   (:body . ,(magit-gh-review-body review)))))
    (when-let ((comments (mapcar #'magit-gh-comment--to-github-format
                                 (magit-gh-review-comments review))))
      (push `(:comments . ,comments) payload))
    (when-let ((event (and (not (equal 'pending (magit-gh-review-state review)))
                           (magit-gh-review-state review))))
      (push `(:event . ,(s-upcase (symbol-name event))) payload))
    (request url
             :type "POST"
             :headers `(("Authorization" . ,(format "token %s" (magit-gh--get-oauth-token)))
                        ("Content-Type" . "application/json")
                        ("Accept" . "application/vnd.github.v3.json"))
             :data (json-encode payload)
             :complete (function*
                        (lambda (&key response &allow-other-keys)
                          (if (not (member (request-response-status-code response) '(200 201)))
                              (error "Failed to submit review to %s" url)))))))

(provide 'magit-gh-comments-github)
