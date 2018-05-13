(require 'ert)

(defstruct magit-gh-diff-pos a-or-b hunk-start offset)

(defun magit-gh--cur-diff-pos ()
  "Calculate the position of point within a magit diff and return
a magit-gh-diff-pos."
  (interactive)
  (save-excursion
    (re-search-backward "^@\\{2,\\} \\(.+?\\) @\\{2,\\}\\(?: \\(.*\\)\\)?"))
  (let* ((heading (match-string 0))
         (irrelevant-section-heading (match-string 2))
         (hunk-ranges (split-string (match-string 1)))
         (rev-a-range (car hunk-ranges))
         (rev-b-range (cadr hunk-ranges))
         (target-line (line-number-at-pos))
         (offset 0)
         (is-looking-at-rev-a  (and rev-a-range rev-b-range
                                    (save-excursion (goto-char (line-beginning-position))
                                                    (looking-at "^-"))))
         (rev-range (if is-looking-at-rev-a
                        rev-a-range
                      rev-b-range))
         (hunk-start (progn
                       (string-match (format "^%s\\([0-9]+\\)" (if is-looking-at-rev-a "-" "\\+")) rev-range)
                       (string-to-number (match-string 1 rev-range)))))
    (save-excursion
      (re-search-backward "^@\\{2,\\} \\(.+?\\) @\\{2,\\}\\(?: \\(.*\\)\\)?")
      (forward-line)
      (beginning-of-line)
      (while (< (line-number-at-pos) target-line)
        (unless (looking-at
                 (format "^%s" (if is-looking-at-rev-a "\\+" "-")))
          (cl-incf offset))
        (assert (= 0 (forward-line))))
      (make-magit-gh-diff-pos :a-or-b (if is-looking-at-rev-a :a :b)
                              :hunk-start hunk-start
                              :offset offset))))

(defun magit-gh--try-parse-hunk-header (&optional line)
  "Read the current line as a hunk-header. Return nil if the line
at point is not a hunk-header"
  (interactive)
  (let ((line (or line (buffer-substring (point-at-bol) (point-at-eol)))))
    (with-temp-buffer
      (insert line)
      (goto-char (point-min))
      (if (looking-at "@@ -\\([0-9]+\\),\\([0-9]+\\) \\+\\([0-9]+\\),\\([0-9]+\\) @@")
          `((:a-start . ,(string-to-number (match-string 1)))
            (:a-len . ,(string-to-number (match-string 2)))
            (:b-start . ,(string-to-number (match-string 3)))
            (:b-len . ,(string-to-number (match-string 4))))))))

(defun magit-gh--partition-by (xs-in fn &optional xs-out)
  "Apply FN to each value in XS-IN, splitting it each time FN
returns a new value. Return a list of lists."
  (if (not xs-in)
      (reverse (cons (reverse (car xs-out)) (cdr xs-out)))
    (cond ((not xs-out)
           (magit-gh--partition-by (cdr xs-in)
                                   fn
                                   (cons (list (car xs-in)) '())))
          ((equal (funcall fn (car xs-in))
                  (funcall fn (caar xs-out)))
           (magit-gh--partition-by (cdr xs-in)
                                   fn
                                   (cons (cons (car xs-in) (car xs-out)) (cdr xs-out))))
          (:t
           (magit-gh--partition-by (cdr xs-in)
                                   fn
                                   (cons (list (car xs-in)) (cons (reverse (car xs-out)) (cdr xs-out))))))))

(ert-deftest test-magit-gh--partition-by ()
  (should (equal '((0 1) (2 3) (4 5))
                 (magit-gh--partition-by '(0 1 2 3 4 5) (lambda (n) (/ n 2)))))
  (should (equal '((:a :b :c) (:d))
                 (magit-gh--partition-by '(:a :b :c :d)  (lambda (x) (eq :d x))))))

(defun magit-gh--pair-and-merge (xs-in &optional xs-out)
  "Pair off adjacent sequences in XS-IN and merge their
contents. Return a sequence of sequences which is half the length
of the original.

Example:

'((:foo) (:bar) (:buzz) (:bop)) -> '((:foo :bar) (:buzz :bop))
"
  (assert (evenp (length xs-in)))
  (if (not xs-in)
      (reverse xs-out)
    (magit-gh--pair-and-merge (cddr xs-in) (cons (append (car xs-in) (cadr xs-in)) xs-out))))

(ert-deftest test-magit-gh--pair-and-merge ()
  (should (equal '((1 2 3 4) (5 6 7 8))
                 (magit-gh--pair-and-merge '((1) (2 3 4) (5 6) (7 8))))))

(defun magit-gh--running-sum (xs)
  "Return a sequence of the same length as XS, where the element
at each index i in the output equals the sum of XS[0..i]

Example:

'(1 2 0) -> '(1 3 3)
"
  (reverse
   (let (result)
     (dolist (x xs result)
       (setq result (cons (+ (or (car result) 0) x)
                          result))))))

(ert-deftest test-magit-gh--running-sum ()
  (should (equal '(1 2 2 4 4 4)
                 (magit-gh--running-sum '(1 1 0 2 0 0))))
  (should (= 5050
             (car (last (magit-gh--running-sum (number-sequence 1 100)))))))

(defun magit-gh--calc-gh-offset (lines pos)
  "Given hunk body LINES and magit diff position POS, calculate
the (Github-style) position within the hunk as 1 + the number of
lines in the hunk that precede POS."
  (let* ((magit-offset (magit-gh-diff-pos-offset pos))
         (char-to-ignore (if (eq (magit-gh-diff-pos-a-or-b pos) :a) ?+ ?-))
         (lines-preceding-target (seq-take-while (lambda (n) (< n (1+ magit-offset)))
                                                (magit-gh--running-sum
                                                 (mapcar (lambda (line)
                                                           (if (not (= char-to-ignore (elt line 0))) 1 0))
                                                         lines)))))
    (if (= (length lines-preceding-target)
             (length lines))
        (error "Error! Position not found")
      (1+ (length lines-preceding-target)))))

(ert-deftest test-magit-gh--calc-gh-offset ()
  (let ((make-pos (lambda (a-or-b offset)
                    (make-magit-gh-diff-pos :a-or-b a-or-b
                                            :offset offset)))
        (lines '("-foo" "+bar" "buzz" "-bam")))
    (should (= 1
               (magit-gh--calc-gh-offset lines (funcall make-pos :a 0))))
    (should (= 2
               (magit-gh--calc-gh-offset lines (funcall make-pos :b 0))))
    (should (= 3
               (magit-gh--calc-gh-offset lines (funcall make-pos :b 1))))
    (should (= 3
               (magit-gh--calc-gh-offset lines (funcall make-pos :a 1))))
    (should (= 4
               (magit-gh--calc-gh-offset lines (funcall make-pos :a 2))))
    (should-error (magit-gh--calc-gh-offset lines (funcall make-pos :b 2)))
    (should-error (magit-gh--calc-gh-offset lines (funcall make-pos :a 3)))))

(defun magit-gh--translate-diff-pos (pos)
  "Given magit diff position POS, calculate the corresponding
Github-style position."
  (let* ((lines (split-string (buffer-string) "\n" t))
         (lines-partitioned-by-hunk-header (cdr ;; discard lines before first hunk header
                                            (magit-gh--partition-by lines 'magit-gh--try-parse-hunk-header)))
         (hunks (magit-gh--pair-and-merge lines-partitioned-by-hunk-header))
         (hunks-not-after-target (seq-take-while
                                  (lambda (hunk)
                                    (let* ((header (magit-gh--try-parse-hunk-header (car hunk)))
                                           (hunk-start (alist-get (if (eq (magit-gh-diff-pos-a-or-b pos) :a)
                                                                      :a-start
                                                                    :b-start)
                                                                  header)))
                                      (<= hunk-start (magit-gh-diff-pos-hunk-start pos))))
                                  hunks))
         (is-valid (assert hunks-not-after-target t
                           (format "Could not find a line at diff position [%s]. Last hunk: [%s]"
                                   pos
                                   (caar (last hunks-not-after-target)))))
         (gh-pos-hunk-start (reduce '+ (mapcar #'length (butlast hunks-not-after-target))))
         (last-hunk-content (cdar (last hunks-not-after-target)))
         (gh-hunk-offset (magit-gh--calc-gh-offset last-hunk-content pos)))
    (+ gh-pos-hunk-start
       gh-hunk-offset)))

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
    (call-process "git" nil buf-name nil "--no-pager" "diff" "-U" num-context-lines "--no-index" "/tmp/a.txt" "/tmp/b.txt")
    buf-name))

(defun magit-gh--generate-magit-diff (rev-a-file rev-b-file)
  (let ((buf-name "*magit-gh-magit-diff*"))
    (when (get-buffer buf-name)
      (kill-buffer buf-name))
    (call-process "git" nil buf-name nil "--no-pager" "diff" "--no-index" rev-a-file rev-b-file)
    buf-name))


(defun magit-gh--pick-random-line-in-diff (diff-buf)
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

(ert-deftest test-magit-gh--translate-diff-pos ()
  (let* ((rev-files (magit-gh--generate-revisions))
         (magit-diff-buf (magit-gh--generate-magit-diff (car rev-files) (cdr rev-files)))
         (random-line-in-diff (magit-gh--pick-random-line-in-diff magit-diff-buf)) ;; TODO: This is a terrible var name
         (diff-pos (alist-get :diff-pos random-line-in-diff))
         (expected-line-contents (alist-get :line-contents random-line-in-diff))
         (github-diff-pos (with-current-buffer magit-diff-buf
                            (magit-gh--translate-diff-pos diff-pos))))
    (should (string= expected-line-contents
                     (magit-gh--line-contents-at-github-pos github-diff-pos
                                                            magit-diff-buf)))))
(ert "test-magit-gh--.*")

