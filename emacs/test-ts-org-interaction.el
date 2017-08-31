;;; test-org-interaction --- Unit tests for my org-interaction library

;;; Commentary:
;; Every library needs a test suite.

;;; Code:
(require 'ert)
(require 'ts-org-interaction)

(defun ts-make-data-from-text (&rest text)
  "Create a parse tree from the Org-formatted TEXT.

Elements of TEXT are assumed to represent sequential lines in the buffer."
  (let ((buffer-text (mapconcat #'identity text "\n"))
        text-mode-hook org-mode-hook)
    (with-temp-buffer
      (org-mode)
      (insert buffer-text)
      (org-element-parse-buffer))))

(defun ts-make-ast-from-text (filename &rest text)
  "Create an entry suitable for *org-ast-list* under FILENAME from TEXT.

Elements of TEXT are assumed to represent sequential lines in the file."
  (cons filename (apply #'ts-make-data-from-text text)))

(defun ts-interpret-data (org-data)
  "Interpret ORG-DATA and return the string with no properties."
  (substring-no-properties (org-element-interpret-data org-data)))

(defun ts-interpret-ast (&rest syntax-tree-alist)
  "Map `org-element-interpret-data' across the data elements of SYNTAX-TREE-ALIST.

Returns an alist of (filename . text)."
  (let ((fn (lambda (elt)
              (cons (car elt)
                    (substring-no-properties
                     (org-element-interpret-data (cdr elt)))))))
    (mapcar fn syntax-tree-alist)))

(defmacro ts-deftest-parametrize (prefix params values &rest body)
  "Create ERT deftests from a list of parameters.

Give them names starting with PREFIX, e.g. PREFIX-0, PREFIX-1, etc.
Bind PARAMS to sequential elements from VALUES and execute test BODY."
  (declare (indent 3))
  (cl-loop for i below (length values)
           collect
           `(ert-deftest ,(intern
                           (concat
                            (symbol-name prefix) "-" (number-to-string i)))
                ()
              (cl-destructuring-bind ,params (list ,@(nth i values))
                ,@body))
           into result
           finally return (cons 'progn result)))

(ts-deftest-parametrize ts-insert-child
    (parent-id left-sibling-id new-hl-plist buffer-data expected-data)
    (("1" nil (list :title "this is the new headline" :id "A")
      (ts-make-data-from-text
       "* TODO this is the parent headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:")
      (ts-make-data-from-text
       "* TODO this is the parent headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:"
       "** TODO this is the new headline"
       ":PROPERTIES:"
       ":ID:       A"
       ":END:"))
     ("1" nil (list :title "this is the new headline" :id "A")
      (ts-make-data-from-text
       "* TODO this is the parent headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:"
       "this is the parent paragraph"
       "** TODO this is the child headline"
       "this is the child paragraph")
      (ts-make-data-from-text
       "* TODO this is the parent headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:"
       "this is the parent paragraph"
       "** TODO this is the new headline"
       ":PROPERTIES:"
       ":ID:       A"
       ":END:"
       "** TODO this is the child headline"
       "this is the child paragraph"))
     ("1" nil (list :title "this is the new headline"
                    :todo-type "done"
                    :id "A")
      (ts-make-data-from-text
       "* This is the parent headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:")
      (ts-make-data-from-text
       "* This is the parent headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:"
       "** DONE this is the new headline"
       ":PROPERTIES:"
       ":ID:       A"
       ":END:"))
     ("1" nil (list :id "A"
                    :title "this is the finished task"
                    :todo-type "DONE"
                    :closed "2017-01-27T22:01:10.030Z")
      (ts-make-data-from-text
       "* TODO this is the parent headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:")
      (ts-make-data-from-text
       "* TODO this is the parent headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:"
       "** DONE this is the finished task"
       "CLOSED: [2017-01-27 Fri 14:01]"
       ":PROPERTIES:"
       ":ID:       A"
       ":END:"))
     ("1" nil (list :id "A"
                    :title "this task has a deadline"
                    :deadline "2017-01-27T22:01:10.030Z")
      (ts-make-data-from-text
       "* TODO this is the parent headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:")
      (ts-make-data-from-text
       "* TODO this is the parent headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:"
       "** TODO this task has a deadline"
       "DEADLINE: <2017-01-27 Fri 14:01>"
       ":PROPERTIES:"
       ":ID:       A"
       ":END:"))
     ("1" nil (list :id "A"
                    :title "this task has a day-only deadline"
                    :deadline "2017-01-27")
      (ts-make-data-from-text
       "* TODO this is the parent headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:")
      (ts-make-data-from-text
       "* TODO this is the parent headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:"
       "** TODO this task has a day-only deadline"
       "DEADLINE: <2017-01-27>"
       ":PROPERTIES:"
       ":ID:       A"
       ":END:"))
     ("1" "B" (list :id "A" :title "this is the new headline")
      (ts-make-data-from-text
       "* TODO this is the grandparent headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:"
       "** TODO this is the parent headline"
       ":PROPERTIES:"
       ":ID:       B"
       ":END:"
       "*** TODO this is the child headline"
       "** TODO this is the next parent headline")
      (ts-make-data-from-text
       "* TODO this is the grandparent headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:"
       "** TODO this is the parent headline"
       ":PROPERTIES:"
       ":ID:       B"
       ":END:"
       "*** TODO this is the child headline"
       "** TODO this is the new headline"
       ":PROPERTIES:"
       ":ID:       A"
       ":END:"
       "** TODO this is the next parent headline"))
     ("1" nil (list :id "A"
                    :title "this task had a deadline"
                    :todo-type "DONE"
                    :deadline "2017-01-27"
                    :closed "2017-01-22T22:53:20.020Z")
      (ts-make-data-from-text
       "* TODO this is the parent headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:")
      (ts-make-data-from-text
       "* TODO this is the parent headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:"
       "** DONE this task had a deadline"
       "DEADLINE: <2017-01-27> CLOSED: [2017-01-22 Sun 14:53]"
       ":PROPERTIES:"
       ":ID:       A"
       ":END:"))
     ("1" nil (list :id "A" :title "this task has tags"
                    :tags '("morning" "@home"))
      (ts-make-data-from-text
       "* TODO this is the parent headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:")
      (ts-make-data-from-text
       "* TODO this is the parent headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:"
       "** TODO this task has tags         :morning:@home:"
       ":PROPERTIES:"
       ":ID:       A"
       ":END:")))
  (let* ((*org-ast-list* (list (cons "tmp" buffer-data)))
         retval)
    (setq retval (ts-insert-child parent-id left-sibling-id new-hl-plist))
    (should (equal (ts-interpret-data buffer-data)
                   (ts-interpret-data expected-data)))
    (should (equal retval (plist-get new-hl-plist :id)))))

(ts-deftest-parametrize ts-insert-child-into-file
    (filename left-sibling-id new-hl-plist ast-data expected-data)
    (("first" nil (list :title "this is the new headline"
                        :paragraph "this is the new paragraph"
                        :id "A")
      (list (ts-make-ast-from-text "first"
                                   "* TODO this is an existing headline"))
      (list (ts-make-ast-from-text "first"
                                   "* TODO this is the new headline"
                                   ":PROPERTIES:"
                                   ":ID:       A"
                                   ":END:"
                                   "this is the new paragraph"
                                   "* TODO this is an existing headline")))
     ("first" "1" (list :title "this is the new headline"
                        :paragraph "this is the new paragraph"
                        :id "A")
      (list (ts-make-ast-from-text "first"
                                   "* TODO this is an existing headline"
                                   ":PROPERTIES:"
                                   ":ID:       1"
                                   ":END:"
                                   "* TODO this headline also exists"))
      (list (ts-make-ast-from-text "first"
                                   "* TODO this is an existing headline"
                                   ":PROPERTIES:"
                                   ":ID:       1"
                                   ":END:"
                                   "* TODO this is the new headline"
                                   ":PROPERTIES:"
                                   ":ID:       A"
                                   ":END:"
                                   "this is the new paragraph"
                                   "* TODO this headline also exists")))
     ("first" nil (list :title "this is the new headline"
                        :paragraph "this is the new paragraph"
                        :id "A")
      (list (ts-make-ast-from-text "first" ""))
      (list (ts-make-ast-from-text "first"
                                   "* TODO this is the new headline"
                                   ":PROPERTIES:"
                                   ":ID:       A"
                                   ":END:"
                                   "this is the new paragraph")))
     ("second" nil (list :title "this is the new headline"
                         :paragraph "this is the new paragraph"
                         :id "A")
      (list (ts-make-ast-from-text "first" "")
            (ts-make-ast-from-text "second" ""))
      (list (ts-make-ast-from-text "first" "")
            (ts-make-ast-from-text "second"
                                   "* TODO this is the new headline"
                                   ":PROPERTIES:"
                                   ":ID:       A"
                                   ":END:"
                                   "this is the new paragraph")))
     ("~/directory/filename.ext" nil (list :title "this is the new headline"
                                           :id "A")
      (list (ts-make-ast-from-text "~/directory/filename.ext" ""))
      (list (ts-make-ast-from-text "~/directory/filename.ext"
                                   "* TODO this is the new headline"
                                   ":PROPERTIES:"
                                   ":ID:       A"
                                   ":END:")))
     (nil nil (list :title "this is the new headline" :id "A")
          (list (ts-make-ast-from-text "first" "")
                (ts-make-ast-from-text "second" ""))
          (list (ts-make-ast-from-text "first"
                                       "* TODO this is the new headline"
                                       ":PROPERTIES:"
                                       ":ID:       A"
                                       ":END:")
                (ts-make-ast-from-text "second" ""))))
  (let ((*org-ast-list* ast-data)
        retval)
    (setq retval (ts-insert-child-into-file filename left-sibling-id
                                            new-hl-plist))
    (should (equal (apply #'ts-interpret-ast ast-data)
                   (apply #'ts-interpret-ast expected-data)))
    (should (equal retval (plist-get new-hl-plist :id)))))

(ts-deftest-parametrize ts-insert-file
    (filename org-directory new-file-plist ast-data expected-data)
    (("new" "/test/directory" (list :project_id "project")
      (list (ts-make-ast-from-text "existing" ""))
      (list (ts-make-ast-from-text "/test/directory/new"
                                   "#+PROJECT_ID: project")
            (ts-make-ast-from-text "existing" ""))))
  (let ((*org-ast-list* ast-data)
        (org-directory org-directory))
    (ts-insert-file filename nil new-file-plist)
    (should (equal (apply #'ts-interpret-ast *org-ast-list*)
                   (apply #'ts-interpret-ast expected-data)))))

(ert-deftest ts-insert-child-no-given-id ()
  (let* ((parent-id "1")
         (left-sibling-id "2")
         (new-hl-plist (list :title "this headline has no given ID"))
         (new-hl-id "4")                ; chosen by fair dice roll
         (buffer-data (ts-make-data-from-text
                       "* This is the parent headline"
                       ":PROPERTIES:"
                       ":ID:       1"
                       ":END:"
                       "** TODO This is a child headline"
                       ":PROPERTIES:"
                       ":ID:       2"
                       ":END:"))
         (expected-data (ts-make-data-from-text
                         "* This is the parent headline"
                         ":PROPERTIES:"
                         ":ID:       1"
                         ":END:"
                         "** TODO This is a child headline"
                         ":PROPERTIES:"
                         ":ID:       2"
                         ":END:"
                         "** TODO this headline has no given ID"
                         ":PROPERTIES:"
                         (concat ":ID:       " new-hl-id)
                         ":END:"))
         (*org-ast-list* (list (cons "tmp" buffer-data)))
         retval)
    (setq retval (unwind-protect
                     (progn
                       (advice-add 'org-id-new :override
                                   (lambda (&optional prefix) new-hl-id)
                                   '((name . "override")))
                       (ts-insert-child parent-id left-sibling-id
                                        new-hl-plist))
                   (advice-remove 'org-id-new "override")))
    (should (equal (ts-interpret-data buffer-data)
                   (ts-interpret-data expected-data)))
    (should (equal retval new-hl-id))))

(ts-deftest-parametrize ts-insert-errors
    (parent-id left-sibling-id new-hl-plist buffer-data)
    (("1" nil (list :paragraph "I have no title")
      (ts-make-data-from-text ""))
     (nil nil (list :title "I have no parent id")
          (ts-make-data-from-text "")))
  (let* ((*org-ast-list* (list (cons "tmp" buffer-data))))
    (should-error (ts-insert-child parent-id left-sibling-id new-hl-plist))))

(ts-deftest-parametrize ts-update
    (id-to-update new-plist buffer-data expected-data)
    (("1" (list :title "this is the new title")
      (ts-make-data-from-text
       "* TODO this is the current headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:")
      (ts-make-data-from-text
       "* TODO this is the new title"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:"))
     ("1" (list :paragraph "this is the new paragraph")
      (ts-make-data-from-text
       "* TODO this is the current headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:"
       "this is the current paragraph")
      (ts-make-data-from-text
       "* TODO this is the current headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:"
       "this is the new paragraph"))
     ("1" (list :paragraph "this is the new paragraph")
      (ts-make-data-from-text
       "* TODO this is the current headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:"
       "this is the first old paragraph"
       ""
       "this is the second old paragraph")
      (ts-make-data-from-text
       "* TODO this is the current headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:"
       "this is the new paragraph"))
     ("1" (list :paragraph "this is the new paragraph")
      (ts-make-data-from-text
       "* TODO this is the current headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:"
       "- a bulleted list"
       "- with two bullets")
      (ts-make-data-from-text
       "* TODO this is the current headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:"
       "this is the new paragraph"))
     ("1" (list :custom_id "A")
      (ts-make-data-from-text
       "* TODO this is the current headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:")
      (ts-make-data-from-text
       "* TODO this is the current headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":CUSTOM_ID: A"
       ":END:"))
     ("1" (list :todo-type "done")
      (ts-make-data-from-text
       "* TODO this is the current headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:")
      (ts-make-data-from-text
       "* DONE this is the current headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:"))
     ("1" (list :todo-type "DONE")
      (ts-make-data-from-text
       "* TODO this is the current headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:")
      (ts-make-data-from-text
       "* DONE this is the current headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:"))
     ("1" (list :todo-type "TODO")
      (ts-make-data-from-text
       "* DONE this is the current headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:")
      (ts-make-data-from-text
       "* TODO this is the current headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:"))
     ("1" (list :todo-type "DONE"
                :closed "2017-01-27T22:01:10.030Z")
      (ts-make-data-from-text
       "* TODO this is the current headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:")
      (ts-make-data-from-text
       "* DONE this is the current headline"
       "CLOSED: [2017-01-27 Fri 14:01]"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:"))
     ("1" (list :todo-type "TODO"
                :closed nil)
      (ts-make-data-from-text
       "* DONE this is the current headline"
       "CLOSED: [2017-01-27 Fri 14:01]"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:")
      (ts-make-data-from-text
       "* TODO this is the current headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:"))
     ("1" (list :paragraph "this is the new paragraph")
      (ts-make-data-from-text
       "* TODO this is the current headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:"
       "** TODO this is the child headline"
       ":PROPERTIES:"
       ":ID:       A"
       ":END:")
      (ts-make-data-from-text
       "* TODO this is the current headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:"
       "this is the new paragraph"
       "** TODO this is the child headline"
       ":PROPERTIES:"
       ":ID:       A"
       ":END:"))
     ("1" (list :todo-type "DONE"
                :closed "2017-01-27T22:01:10.030Z")
      (ts-make-data-from-text
       "* TODO this is the current headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:"
       "** DONE this is the child headline"
       "CLOSED: [2017-01-26 Thu 10:00]"
       ":PROPERTIES:"
       ":ID:       A"
       ":END:")
      (ts-make-data-from-text
       "* DONE this is the current headline"
       "CLOSED: [2017-01-27 Fri 14:01]"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:"
       "** DONE this is the child headline"
       "CLOSED: [2017-01-26 Thu 10:00]"
       ":PROPERTIES:"
       ":ID:       A"
       ":END:"))
     ("1" (list :todo-type "TODO" :closed nil)
      (ts-make-data-from-text
       "* DONE this is the current headline"
       "CLOSED: [2017-01-27 Fri 14:01]"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:"
       "** DONE this is the child headline"
       "CLOSED: [2017-01-26 Thu 10:00]"
       ":PROPERTIES:"
       ":ID:       A"
       ":END:")
      (ts-make-data-from-text
       "* TODO this is the current headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:"
       "** DONE this is the child headline"
       "CLOSED: [2017-01-26 Thu 10:00]"
       ":PROPERTIES:"
       ":ID:       A"
       ":END:"))
     ("1" (list :deadline "2017-01-27T22:01:10.030Z")
      (ts-make-data-from-text
       "* TODO this is the current headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:")
      (ts-make-data-from-text
       "* TODO this is the current headline"
       "DEADLINE: <2017-01-27 Fri 14:01>"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:"))
     ("1" (list :deadline "2017-01-27")
      (ts-make-data-from-text
       "* TODO this is the current headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:")
      (ts-make-data-from-text
       "* TODO this is the current headline"
       "DEADLINE: <2017-01-27>"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:"))
     ("1" (list :deadline "2017-01-27")
      (ts-make-data-from-text
       "* TODO this is the current headline"
       "SCHEDULED: <2017-01-20>"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:")
      (ts-make-data-from-text
       "* TODO this is the current headline"
       "SCHEDULED: <2017-01-20> DEADLINE: <2017-01-27>"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:"))
     ("1" (list :deadline nil)
      (ts-make-data-from-text
       "* TODO this is the current headline"
       "DEADLINE: <2017-01-27 Fri 14:01>"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:")
      (ts-make-data-from-text
       "* TODO this is the current headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:"))
     ("1" (list :deadline nil)
      (ts-make-data-from-text
       "* TODO this is the current headline"
       "SCHEDULED: <2017-01-20> DEADLINE: <2017-01-27>"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:")
      (ts-make-data-from-text
       "* TODO this is the current headline"
       "SCHEDULED: <2017-01-20>"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:"))
     ("1" (list :custom_id "already_existed")
      (ts-make-data-from-text
       "* TODO a headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":CUSTOM_ID: A"
       ":END:")
      (ts-make-data-from-text
       "* TODO a headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":CUSTOM_ID: already_existed"
       ":END:"))
     ("1" (list :tags '("morning" "@home"))
      (ts-make-data-from-text
       "* TODO a headline"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:")
      (ts-make-data-from-text
       "* TODO a headline                   :morning:@home:"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:"))
     ("1" (list :tags ())
      (ts-make-data-from-text
       "* TODO a headline with tags         :@home:"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:")
      (ts-make-data-from-text
       "* TODO a headline with tags"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:")))
  (let ((*org-ast-list* (list (cons "tmp" buffer-data))))
    (ts-update id-to-update new-plist)
    (should (equal (ts-interpret-data buffer-data)
                   (ts-interpret-data expected-data)))))

(ts-deftest-parametrize ts-update-file
    (filename new-plist ast-data expected-data)
    (("first" (list :project_id "A")
      (list (ts-make-ast-from-text "first" ""))
      (list (ts-make-ast-from-text "first" "#+PROJECT_ID: A")))
     ("first" (list :project_id "B")
      (list (ts-make-ast-from-text "first" "#+PROJECT_ID: A"))
      (list (ts-make-ast-from-text "first" "#+PROJECT_ID: B")))
     ("inbox" (list :project_id "12345")
      (list (ts-make-ast-from-text "inbox" "#+ARCHIVE: %s_archive::"))
      (list (ts-make-ast-from-text "inbox"
                                   "#+ARCHIVE: %s_archive::"
                                   "#+PROJECT_ID: 12345")))
     ("inbox" (list :project_id "12345")
      (list (ts-make-ast-from-text "inbox"
                                   "#+PROPERTY: LOGGING lognotedone"
                                   "An opening sentence."
                                   "* TODO the first headline"))
      (list (ts-make-ast-from-text "inbox"
                                   "#+PROPERTY: LOGGING lognotedone"
                                   "An opening sentence."
                                   "#+PROJECT_ID: 12345"
                                   "* TODO the first headline")))
     ("inbox" (list :project_id "12345")
      (list (ts-make-ast-from-text "inbox"
                                   "#+PROJECT_ID: A"
                                   "An opening sentence."
                                   "* TODO the first headline"))
      (list (ts-make-ast-from-text "inbox"
                                   "#+PROJECT_ID: 12345"
                                   "An opening sentence."
                                   "* TODO the first headline"))))
  (let ((*org-ast-list* ast-data))
    (ts-update-file filename new-plist)
    (should (equal (apply #'ts-interpret-ast ast-data)
                   (apply #'ts-interpret-ast expected-data)))))

(ts-deftest-parametrize ts-move-to
    (child-id left-sibling-id new-parent-id buffer-data expected-data)
    (("A" nil "2"
      (ts-make-data-from-text
       "* TODO this is the old parent headline"
       "** TODO this is the child headline"
       ":PROPERTIES:"
       ":ID:       A"
       ":END:"
       "* TODO this is the new parent headline"
       ":PROPERTIES:"
       ":ID:       2"
       ":END:")
      (ts-make-data-from-text
       "* TODO this is the old parent headline"
       "* TODO this is the new parent headline"
       ":PROPERTIES:"
       ":ID:       2"
       ":END:"
       "** TODO this is the child headline"
       ":PROPERTIES:"
       ":ID:       A"
       ":END:"))
     ("A" "3" "2"
      (ts-make-data-from-text
       "* TODO this is the old parent headline"
       "** TODO this is the reparented headline"
       ":PROPERTIES:"
       ":ID:       A"
       ":END:"
       "* TODO this is the new parent headline"
       ":PROPERTIES:"
       ":ID:       2"
       ":END:"
       "** TODO this is the existing child headline"
       ":PROPERTIES:"
       ":ID:       3"
       ":END:"
       "*** TODO this is the existing baby headline"
       "** TODO this is the next child headline")
      (ts-make-data-from-text
       "* TODO this is the old parent headline"
       "* TODO this is the new parent headline"
       ":PROPERTIES:"
       ":ID:       2"
       ":END:"
       "** TODO this is the existing child headline"
       ":PROPERTIES:"
       ":ID:       3"
       ":END:"
       "*** TODO this is the existing baby headline"
       "** TODO this is the reparented headline"
       ":PROPERTIES:"
       ":ID:       A"
       ":END:"
       "** TODO this is the next child headline")))
  (let ((*org-ast-list* (list (cons "tmp" buffer-data))))
    (ts-move-to child-id left-sibling-id new-parent-id)
    (should (equal (ts-interpret-data buffer-data)
                   (ts-interpret-data expected-data)))))

(ts-deftest-parametrize ts-move-to-file
    (child-id left-sibling-id new-filename ast-data expected-data)
    (("A" nil "second"
      (list (ts-make-ast-from-text "first"
                                   "* TODO this headline moves"
                                   ":PROPERTIES:"
                                   ":ID:       A"
                                   ":END:")
            (ts-make-ast-from-text "second"
                                   "* TODO already here"))
      (list (ts-make-ast-from-text "first" "")
            (ts-make-ast-from-text "second"
                                   "* TODO this headline moves"
                                   ":PROPERTIES:"
                                   ":ID:       A"
                                   ":END:"
                                   "* TODO already here")))
     ("B" "1" "second"
      (list (ts-make-ast-from-text "first"
                                   "* TODO this headline stays"
                                   ":PROPERTIES:"
                                   ":ID:       A"
                                   ":END:"
                                   "* TODO this headline moves"
                                   ":PROPERTIES:"
                                   ":ID:       B"
                                   ":END:")
            (ts-make-ast-from-text "second"
                                   "* TODO already here"
                                   ":PROPERTIES:"
                                   ":ID:       1"
                                   ":END:"
                                   "* TODO gonna get pushed"))
      (list (ts-make-ast-from-text "first"
                                   "* TODO this headline stays"
                                   ":PROPERTIES:"
                                   ":ID:       A"
                                   ":END:")
            (ts-make-ast-from-text "second"
                                   "* TODO already here"
                                   ":PROPERTIES:"
                                   ":ID:       1"
                                   ":END:"
                                   "* TODO this headline moves"
                                   ":PROPERTIES:"
                                   ":ID:       B"
                                   ":END:"
                                   "* TODO gonna get pushed")))
     ("A" nil nil
      (list (ts-make-ast-from-text "first")
            (ts-make-ast-from-text "second"
                                   "* TODO this headline moves"
                                   ":PROPERTIES:"
                                   ":ID:       A"
                                   ":END:"))
      (list (ts-make-ast-from-text "first"
                                   "* TODO this headline moves"
                                   ":PROPERTIES:"
                                   ":ID:       A"
                                   ":END:")
            (ts-make-ast-from-text "second"))))
  (let ((*org-ast-list* ast-data))
    (ts-move-to-file child-id left-sibling-id new-filename)
    (should (equal (apply #'ts-interpret-ast ast-data)
                   (apply #'ts-interpret-ast expected-data)))))

(ert-deftest ts-delete ()
  (let* ((id-to-delete "1")
         (buffer-data (ts-make-data-from-text
                       "* TODO this is the remaining headline"
                       "** TODO this is the headline to delete"
                       ":PROPERTIES:"
                       ":ID:       1"
                       ":END:"))
         (expected-data (ts-make-data-from-text
                         "* TODO this is the remaining headline"))
         (*org-ast-list* (list (cons "tmp" buffer-data))))
    (ts-delete id-to-delete)
    (should (equal (ts-interpret-data buffer-data)
                   (ts-interpret-data expected-data)))))

(ts-deftest-parametrize ts-delete-file
    (filename ast-data expected-data)
    (("bye"
      (list (ts-make-ast-from-text "bye" ""))
      nil)
     ("bye"
      (list (ts-make-ast-from-text "bye" "* TODO I'm gone")
            (ts-make-ast-from-text "remain" ""))
      (list (ts-make-ast-from-text "remain" ""))))
  (let ((*org-ast-list* ast-data))
    (ts-delete-file filename)
    (should (equal (apply #'ts-interpret-ast *org-ast-list*)
                   (apply #'ts-interpret-ast expected-data)))))

(ts-deftest-parametrize ts-get-all-headlines
    (field-list buffer-data expected-text)
    (('(:ID)
      (ts-make-data-from-text "* TODO this is the only headline")
      "[{'id': None}]")
     ('(:id)
      (ts-make-data-from-text
       "* TODO this is the only headline"
       ":PROPERTIES:"
       ":ID:       A"
       ":END:")
      "[{'id': \"A\"}]")
     ('(:custom_id)
      (ts-make-data-from-text
       "* TODO this is the only headline"
       ":PROPERTIES:"
       ":CUSTOM_ID: A"
       ":END:")
      "[{'custom_id': \"A\"}]")
     ('(:paragraph)
      (ts-make-data-from-text "* TODO this is the only headline")
      "[{'paragraph': \"\"}]")
     ('(:title)
      (ts-make-data-from-text "* TODO this is the only headline")
      "[{'title': \"this is the only headline\"}]")
     ('(:id)
      (ts-make-data-from-text
       "* TODO this is the root headline"
       "** TODO this is the first headline"
       ":PROPERTIES:"
       ":ID:       A"
       ":END:"
       "** TODO this is the second headline"
       ":PROPERTIES:"
       ":ID:       B"
       ":END:")
      "[{'id': None}, {'id': \"A\"}, {'id': \"B\"}]")
     ('(:id :parent-id)
      (ts-make-data-from-text
       "* TODO this is the parent headline"
       ":PROPERTIES:"
       ":ID:       A"
       ":END:"
       "** TODO this is the child headline"
       ":PROPERTIES:"
       ":ID:       B"
       ":END:")
      "[{'id': \"A\", 'parent_id': None}, {'id': \"B\", 'parent_id': \"A\"}]")
     ('(:paragraph)
      (ts-make-data-from-text
       "* TODO this is the headline"
       "A string with several"
       ""
       "newlines in it.")
      "[{'paragraph': \"A string with several\\n\\nnewlines in it.\"}]")
     ('(:paragraph)
      (ts-make-data-from-text
       "* TODO this is the headline"
       "A string with a"
       "newline in it.")
      "[{'paragraph': \"A string with a\\nnewline in it.\"}]")
     ('(:paragraph)
      (ts-make-data-from-text
       "* TODO this is the headline"
       "- a bulleted list")
      "[{'paragraph': \"- a bulleted list\"}]")
     ('(:todo-type)
      (ts-make-data-from-text "* TODO this is the headline")
      "[{'todo_type': \"TODO\"}]")
     ('(:id)
      (ts-make-data-from-text
       "* TODO this is a todo headline"
       ":PROPERTIES:"
       ":ID:       A"
       ":END:"
       "* DONE this is a done headline"
       ":PROPERTIES:"
       ":ID:       B"
       ":END:")
      "[{'id': \"A\"}, {'id': \"B\"}]")
     ('(:closed)
      (ts-make-data-from-text
       "* TODO this headline is open"
       "* DONE this headline is closed"
       "CLOSED: [2016-11-19 Sat 19:34]")
      "[{'closed': None}, {'closed': \"2016-11-20T03:34:00.000Z\"}]")
     ('(:deadline)
      (ts-make-data-from-text
       "* TODO this headline has no deadline"
       "* TODO this headline has a deadline"
       "DEADLINE: <2016-11-19 Sat 19:34>")
      "[{'deadline': None}, {'deadline': \"2016-11-20T03:34:00.000Z\"}]")
     ('(:deadline)
      (ts-make-data-from-text
       "* TODO this headline has no deadline"
       "* TODO this headline has a day-only deadline"
       "DEADLINE: <2016-11-19>")
      "[{'deadline': None}, {'deadline': \"2016-11-19\"}]")
     ('(:paragraph)
      (ts-make-data-from-text
       "* TODO this parent headline has no text"
       "** TODO this child headline has text"
       "just the text")
      "[{'paragraph': \"\"}, {'paragraph': \"just the text\"}]")
     ('(:id :parent-id)
      (ts-make-data-from-text
       "* DONE this parent is complete"
       ":PROPERTIES:"
       ":ID:       1"
       ":END:"
       "** DONE this child is complete as well"
       ":PROPERTIES:"
       ":ID:       2"
       ":END:")
      (concat
       "[{'id': \"1\", 'parent_id': None},"
       " {'id': \"2\", 'parent_id': \"1\"}]"))
     ('(:title)
      (ts-make-data-from-text "* TODO a_headline_with_underscores")
      "[{'title': \"a_headline_with_underscores\"}]")
     ('(:paragraph)
      (ts-make-data-from-text
       "* TODO this headline has a code block in it"
       "#+BEGIN_SRC emacs-lisp"
       "  (+ 2 2)"
       "#+END_SRC")
      "[{'paragraph': \"#+BEGIN_SRC emacs-lisp\\n  (+ 2 2)\\n#+END_SRC\"}]")
     ('(:tags)
      (ts-make-data-from-text "* DONE no tags here")
      "[{'tags': None}]")
     ('(:tags)
      (ts-make-data-from-text "* TODO get things done :morning:")
      "[{'tags': [\"morning\"]}]")
     ('(:tags)
      (ts-make-data-from-text "* TODO get more done :morning:@computer:")
      "[{'tags': [\"morning\", \"@computer\"]}]")
     ('(:tags)
      (ts-make-data-from-text
       "* TODO get this done :morning:"
       "* TODO get that done :evening:")
      "[{'tags': [\"morning\"]}, {'tags': [\"evening\"]}]"))
  (let ((*org-ast-list* (list (cons "tmp" buffer-data))))
    (should (equal (ts-get-all-headlines field-list) expected-text))))

(ts-deftest-parametrize ts-get-all-headlines-filename
    (ast-list expected-text)
    (((list (ts-make-ast-from-text "tmp"
                                   "* TODO just some dummy text"))
      "[{'filename': \"tmp\"}]")
     ((list (ts-make-ast-from-text "~/directories/filename.ext"
                                   "* TODO just some dummy text"))
      "[{'filename': \"~/directories/filename.ext\"}]")
     ((list (ts-make-ast-from-text "first"
                                   "* TODO first parent"
                                   "** TODO child")
            (ts-make-ast-from-text "second"
                                   "* TODO second parent"))
      (concat
       "[{'filename': \"first\"},"
       " {'filename': \"first\"},"
       " {'filename': \"second\"}]")))
  (let ((*org-ast-list* ast-list))
    (should (equal (ts-get-all-headlines '(:filename)) expected-text))))

(ts-deftest-parametrize ts-get-all-filenames
    (field-list ast-list expected-text)
    ((nil
      (list (ts-make-ast-from-text "test"
                                   "* TODO a test headline")
            (ts-make-ast-from-text "~/directory/file.ext"
                                   "* TODO another headline"))
      "[{'id': \"test\"}, {'id': \"~/directory/file.ext\"}]")
     ((list :custom_id)
      (list (ts-make-ast-from-text "test"
                                   "#+CUSTOM_ID: value"
                                   "* TODO a headline also with the id"
                                   ":PROPERTIES:"
                                   ":CUSTOM_ID: A"
                                   ":END:"))
      "[{'id': \"test\", 'custom_id': \"value\"}]"))
  (let ((*org-ast-list* ast-list))
    (should (equal (ts-get-all-filenames field-list) expected-text))))

(provide 'test-ts-org-interaction)
;;; test-ts-org-interaction.el ends here
