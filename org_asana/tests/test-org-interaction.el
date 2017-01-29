;;; test-org-interaction --- Unit tests for my org-interaction library

;;; Commentary:
;; Every library needs a test suite.

;;; Code:
(require 'ert)
(require 'org-interaction)

(defun oi-make-data-from-text (text)
  "Create a parse tree from the Org-formatted TEXT."
  (let (org-mode-hook)
    (with-temp-buffer
      (org-mode)
      (insert text)
      (org-element-parse-buffer))))

(defun oi-make-headline-from-text (text)
  "Create a headline from the Org-formatted TEXT."
  (org-element-map (oi-make-data-from-text text)
      'headline #'identity nil t))

(defun oi-concat-with-newlines (&rest args)
  "Concatenate ARGS with newlines in between."
  (mapconcat #'identity args "\n"))

(defun oi-make-drawer-text-from-plist (plist)
  "Create Org property drawer text with keys & values from PLIST."
  (let (k v retval)
    (push ":PROPERTIES:" retval)
    (while plist
      (setq k (pop plist) v (pop plist))
      (push (concat (upcase (symbol-name k)) ":"
                    (make-string (max 1 (- 10 (length (symbol-name k))))
                                 ? )
                    v)
            retval))
    (push ":END:" retval)
    (apply 'oi-concat-with-newlines (nreverse retval))))

(ert-deftest oi-insert-child ()
  "Does `oi-insert-child' insert children correctly?"
  (let* ((parent-id "1")
         (position 0)
         (new-hl-plist (list :title "this is the new headline"
                             :id "A"))
         (parent-text (oi-concat-with-newlines
                       "* TODO this is the parent headline"
                       ":PROPERTIES:"
                       ":ID:       1"
                       ":END:"))
         (expected-text (oi-concat-with-newlines
                         "* TODO this is the parent headline"
                         ":PROPERTIES:"
                         ":ID:       1"
                         ":END:"
                         "** TODO this is the new headline"
                         ":PROPERTIES:"
                         ":ID:       A"
                         ":END:"))
         (parent-hl (oi-make-headline-from-text parent-text))
         (expected-hl (oi-make-headline-from-text expected-text))
         retval
         (*org-ast-list* (list (cons "tmp" parent-hl))))
    (setq retval (oi-insert-child parent-id position new-hl-plist)
          parent-hl (oi-get-headline-from-id parent-id *org-ast-list*))
    (should (equal (org-element-interpret-data parent-hl)
                   (org-element-interpret-data expected-hl)))
    (should (equal retval (plist-get new-hl-plist :id))))
  (let* ((parent-id "1")
         (position 0)
         (new-hl-plist (list :title "this is the new headline" :id "A"))
         (parent-text (oi-concat-with-newlines
                       "* TODO this is the parent headline"
                       ":PROPERTIES:"
                       ":ID:       1"
                       ":END:"
                       "this is the parent paragraph"
                       "** TODO this is the child headline"
                       "this is the child paragraph"))
         (expected-text (oi-concat-with-newlines
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
         (parent-hl (oi-make-headline-from-text parent-text))
         (expected-hl (oi-make-headline-from-text expected-text))
         retval
         (*org-ast-list* (list (cons "tmp" parent-hl))))
    (setq retval (oi-insert-child parent-id position new-hl-plist)
          parent-hl (oi-get-headline-from-id parent-id *org-ast-list*))
    (should (equal (org-element-interpret-data parent-hl)
                   (org-element-interpret-data expected-hl)))
    (should (equal retval (plist-get new-hl-plist :id))))
  (let* ((parent-id nil)
         (position 0)
         (new-hl-plist (list :title "this is the new headline"
                             :paragraph "this is the new paragraph"
                             :id "A"))
         (buffer-text "* TODO this is an existing headline")
         (expected-text (oi-concat-with-newlines
                         "* TODO this is the new headline"
                         ":PROPERTIES:"
                         ":ID:       A"
                         ":END:"
                         "this is the new paragraph"
                         "* TODO this is an existing headline"))
         (buffer-data (oi-make-data-from-text buffer-text))
         (expected-data (oi-make-data-from-text expected-text))
         retval
         (*org-ast-list* (list (cons "tmp" buffer-data))))
    (setq retval (oi-insert-child parent-id position new-hl-plist))
    (should (equal (org-element-interpret-data buffer-data)
                   (org-element-interpret-data expected-data)))
    (should (equal retval (plist-get new-hl-plist :id))))
  (let* ((parent-id nil)
         (position 0)
         (new-hl-plist (list :title "this is the new headline"
                             :paragraph "this is the new paragraph"
                             :id "A"))
         (buffer-text "")
         (expected-text (oi-concat-with-newlines
                         "* TODO this is the new headline"
                         ":PROPERTIES:"
                         ":ID:       A"
                         ":END:"
                         "this is the new paragraph"))
         (buffer-data (oi-make-data-from-text buffer-text))
         (expected-data (oi-make-data-from-text expected-text))
         retval
         (*org-ast-list* (list (cons "tmp" buffer-data))))
    (setq retval (oi-insert-child parent-id position new-hl-plist))
    (should (equal (org-element-interpret-data buffer-data)
                   (org-element-interpret-data expected-data)))
    (should (equal retval (plist-get new-hl-plist :id))))
  (let* ((parent-id "1")
         (position 0)
         (new-hl-plist (list :title "this headline has no given ID"))
         (parent-text (oi-concat-with-newlines
                       "* This is the parent headline"
                       ":PROPERTIES:"
                       ":ID:       1"
                       ":END:"))
         (expected-text (oi-concat-with-newlines
                         "* This is the parent headline"
                         ":PROPERTIES:"
                         ":ID:       1"
                         ":END:"
                         "** TODO this headline has no given ID"
                         ":PROPERTIES:"
                         ":ID:       2"
                         ":END:"))
         (parent-hl (oi-make-headline-from-text parent-text))
         (expected-hl (oi-make-headline-from-text expected-text))
         retval
         (*org-ast-list* (list (cons "tmp" parent-hl))))
    (setq retval
          (unwind-protect
              (progn
                (advice-add 'org-id-get-create :override
                            #'(lambda ()
                                (org-entry-put (point) "ID" "2"))
                            '((name . "override")))
                (oi-insert-child parent-id position new-hl-plist))
            (advice-remove 'org-id-get-create "override")))
    (should (equal (org-element-interpret-data parent-hl)
                   (org-element-interpret-data expected-hl)))
    (should (equal retval "2")))
  (let* ((parent-id "1")
         (position 0)
         (new-hl-plist (list :title "this is the new headline"
                             :todo-keyword "done"
                             :id "A"))
         (parent-text (oi-concat-with-newlines
                       "* This is the parent headline"
                       ":PROPERTIES:"
                       ":ID:       1"
                       ":END:"))
         (expected-text (oi-concat-with-newlines
                         "* This is the parent headline"
                         ":PROPERTIES:"
                         ":ID:       1"
                         ":END:"
                         "** DONE this is the new headline"
                         ":PROPERTIES:"
                         ":ID:       A"
                         ":END:"))
         (parent-hl (oi-make-headline-from-text parent-text))
         (expected-hl (oi-make-headline-from-text expected-text))
         retval
         (*org-ast-list* (list (cons "tmp" parent-hl))))
    (setq retval (oi-insert-child parent-id position new-hl-plist)
          parent-hl (oi-get-headline-from-id parent-id *org-ast-list*))
    (should (equal (org-element-interpret-data parent-hl)
                   (org-element-interpret-data expected-hl)))
    (should (equal retval (plist-get new-hl-plist :id))))
  (let* ((parent-id "1")
         (position 0)
         (new-hl-plist (list :id "A"
                             :title "this is the finished task"
                             :todo-keyword "DONE"
                             :closed "2017-01-27T22:01:10.030Z"))
         (parent-text (oi-concat-with-newlines
                       "* TODO this is the parent headline"
                       ":PROPERTIES:"
                       ":ID:       1"
                       ":END:"))
         (expected-text (oi-concat-with-newlines
                         "* TODO this is the parent headline"
                         ":PROPERTIES:"
                         ":ID:       1"
                         ":END:"
                         "** DONE this is the finished task"
                         "CLOSED: [2017-01-27 Fri 14:01]"
                         ":PROPERTIES:"
                         ":ID:       A"
                         ":END:"))
         (parent-hl (oi-make-headline-from-text parent-text))
         (expected-hl (oi-make-headline-from-text expected-text))
         retval
         (*org-ast-list* (list (cons "tmp" parent-hl))))
    (setq retval (oi-insert-child parent-id position new-hl-plist)
          parent-hl (oi-get-headline-from-id parent-id *org-ast-list*))
    (should (equal (org-element-interpret-data parent-hl)
                   (org-element-interpret-data expected-hl)))
    (should (equal retval (plist-get new-hl-plist :id))))
  (let* ((parent-id "1")
         (position 1)
         (new-hl-plist (list :id "A" :title "this is the new headline"))
         (parent-text (oi-concat-with-newlines
                       "* TODO this is the grandparent headline"
                       ":PROPERTIES:"
                       ":ID:       1"
                       ":END:"
                       "** TODO this is the parent headline"
                       "*** TODO this is the child headline"
                       "** TODO this is the next parent headline"))
         (expected-text (oi-concat-with-newlines
                         "* TODO this is the grandparent headline"
                         ":PROPERTIES:"
                         ":ID:       1"
                         ":END:"
                         "** TODO this is the parent headline"
                         "*** TODO this is the child headline"
                         "** TODO this is the new headline"
                         ":PROPERTIES:"
                         ":ID:       A"
                         ":END:"
                         "** TODO this is the next parent headline"))
         (parent-hl (oi-make-headline-from-text parent-text))
         (expected-hl (oi-make-headline-from-text expected-text))
         retval
         (*org-ast-list* (list (cons "tmp" parent-hl))))
    (setq retval (oi-insert-child parent-id position new-hl-plist)
          parent-hl (oi-get-headline-from-id parent-id *org-ast-list*))
    (should (equal (org-element-interpret-data parent-hl)
                   (org-element-interpret-data expected-hl)))
    (should (equal retval (plist-get new-hl-plist :id)))))

(ert-deftest oi-delete ()
  "Does `oi-delete' delete headlines correctly?"
  (let* ((id-to-delete "1")
         (hl-text (oi-concat-with-newlines
                   "* TODO this is the remaining headline"
                   "** TODO this is the headline to delete"
                   ":PROPERTIES:"
                   ":ID:       1"
                   ":END:"))
         (expected-text "* TODO this is the remaining headline")
         (hl (oi-make-headline-from-text hl-text))
         (expected-hl (oi-make-headline-from-text expected-text))
         (*org-ast-list* (list (cons "tmp" hl))))
    (oi-delete id-to-delete)
    (should (equal (org-element-interpret-data hl)
                   (org-element-interpret-data expected-hl)))))

(ert-deftest oi-update ()
  "Does `oi-update' update properties correctly?"
  (let* ((id-to-update "1")
         (new-plist (list :title "this is the new title"))
         (hl-text (oi-concat-with-newlines
                   "* TODO this is the current headline"
                   ":PROPERTIES:"
                   ":ID:       1"
                   ":END:"))
         (expected-text (oi-concat-with-newlines
                         "* TODO this is the new title"
                         ":PROPERTIES:"
                         ":ID:       1"
                         ":END:"))
         (hl (oi-make-headline-from-text hl-text))
         (expected-hl (oi-make-headline-from-text expected-text))
         (*org-ast-list* (list (cons "tmp" hl))))
    (oi-update id-to-update new-plist)
    (should (equal (org-element-interpret-data hl)
                   (org-element-interpret-data expected-hl))))
  (let* ((id-to-update "1")
         (new-plist (list :paragraph "this is the new paragraph"))
         (hl-text (oi-concat-with-newlines
                   "* TODO this is the current headline"
                   ":PROPERTIES:"
                   ":ID:       1"
                   ":END:"
                   "this is the current paragraph"))
         (expected-text (oi-concat-with-newlines
                         "* TODO this is the current headline"
                         ":PROPERTIES:"
                         ":ID:       1"
                         ":END:"
                         "this is the new paragraph"))
         (hl (oi-make-headline-from-text hl-text))
         (expected-hl (oi-make-headline-from-text expected-text))
         (*org-ast-list* (list (cons "tmp" hl))))
    (oi-update id-to-update new-plist)
    (should (equal (org-element-interpret-data hl)
                   (org-element-interpret-data expected-hl))))
  (let* ((id-to-update "1")
         (new-plist (list :paragraph "this is the new paragraph"))
         (hl-text (oi-concat-with-newlines
                   "* TODO this is the current headline"
                   ":PROPERTIES:"
                   ":ID:       1"
                   ":END:"
                   "this is the first old paragraph"
                   ""
                   "this is the second old paragraph"))
         (expected-text (oi-concat-with-newlines
                         "* TODO this is the current headline"
                         ":PROPERTIES:"
                         ":ID:       1"
                         ":END:"
                         "this is the new paragraph"))
         (hl (oi-make-headline-from-text hl-text))
         (expected-hl (oi-make-headline-from-text expected-text))
         (*org-ast-list* (list (cons "tmp" hl))))
    (oi-update id-to-update new-plist)
    (should (equal (org-element-interpret-data hl)
                   (org-element-interpret-data expected-hl))))
  (let* ((id-to-update "1")
         (new-plist (list :paragraph "this is the new paragraph"))
         (hl-text (oi-concat-with-newlines
                   "* TODO this is the current headline"
                   ":PROPERTIES:"
                   ":ID:       1"
                   ":END:"
                   "- a bulleted list"
                   "- with two bullets"))
         (expected-text (oi-concat-with-newlines
                         "* TODO this is the current headline"
                         ":PROPERTIES:"
                         ":ID:       1"
                         ":END:"
                         "this is the new paragraph"))
         (hl (oi-make-headline-from-text hl-text))
         (expected-hl (oi-make-headline-from-text expected-text))
         (*org-ast-list* (list (cons "tmp" hl))))
    (oi-update id-to-update new-plist)
    (should (equal (org-element-interpret-data hl)
                   (org-element-interpret-data expected-hl))))
  (let* ((id-to-update "1")
         (new-plist (list :custom_id "A"))
         (hl-text (oi-concat-with-newlines
                   "* TODO this is the current headline"
                   ":PROPERTIES:"
                   ":ID:       1"
                   ":END:"))
         (expected-text (oi-concat-with-newlines
                         "* TODO this is the current headline"
                         ":PROPERTIES:"
                         ":ID:       1"
                         ":CUSTOM_ID: A"
                         ":END:"))
         (hl (oi-make-headline-from-text hl-text))
         (expected-hl (oi-make-headline-from-text expected-text))
         (*org-ast-list* (list (cons "tmp" hl))))
    (oi-update id-to-update new-plist)
    (should (equal (org-element-interpret-data hl)
                   (org-element-interpret-data expected-hl))))
  (let* ((id-to-update "1")
         (new-plist (list :todo-keyword "done"))
         (hl-text (oi-concat-with-newlines
                   "* TODO this is the current headline"
                   ":PROPERTIES:"
                   ":ID:       1"
                   ":END:"))
         (expected-text (oi-concat-with-newlines
                         "* DONE this is the current headline"
                         ":PROPERTIES:"
                         ":ID:       1"
                         ":END:"))
         (hl (oi-make-headline-from-text hl-text))
         (expected-hl (oi-make-headline-from-text expected-text))
         (*org-ast-list* (list (cons "tmp" hl))))
    (oi-update id-to-update new-plist)
    (should (equal (org-element-interpret-data hl)
                   (org-element-interpret-data expected-hl))))
  (let* ((id-to-update "1")
         (new-plist (list :todo-keyword "DONE"))
         (hl-text (oi-concat-with-newlines
                   "* TODO this is the current headline"
                   ":PROPERTIES:"
                   ":ID:       1"
                   ":END:"))
         (expected-text (oi-concat-with-newlines
                         "* DONE this is the current headline"
                         ":PROPERTIES:"
                         ":ID:       1"
                         ":END:"))
         (hl (oi-make-headline-from-text hl-text))
         (expected-hl (oi-make-headline-from-text expected-text))
         (*org-ast-list* (list (cons "tmp" hl))))
    (oi-update id-to-update new-plist)
    (should (equal (org-element-interpret-data hl)
                   (org-element-interpret-data expected-hl))))
  (let* ((id-to-update "1")
         (new-plist (list :todo-keyword "TODO"))
         (hl-text (oi-concat-with-newlines
                   "* DONE this is the current headline"
                   ":PROPERTIES:"
                   ":ID:       1"
                   ":END:"))
         (expected-text (oi-concat-with-newlines
                         "* TODO this is the current headline"
                         ":PROPERTIES:"
                         ":ID:       1"
                         ":END:"))
         (hl (oi-make-headline-from-text hl-text))
         (expected-hl (oi-make-headline-from-text expected-text))
         (*org-ast-list* (list (cons "tmp" hl))))
    (oi-update id-to-update new-plist)
    (should (equal (org-element-interpret-data hl)
                   (org-element-interpret-data expected-hl))))
  (let* ((id-to-update "1")
         (new-plist (list :todo-keyword "DONE"
                          :closed "2017-01-27T22:01:10.030Z"))
         (hl-text (oi-concat-with-newlines
                   "* TODO this is the current headline"
                   ":PROPERTIES:"
                   ":ID:       1"
                   ":END:"))
         (expected-text (oi-concat-with-newlines
                         "* DONE this is the current headline"
                         "CLOSED: [2017-01-27 Fri 14:01]"
                         ":PROPERTIES:"
                         ":ID:       1"
                         ":END:"))
         (hl (oi-make-headline-from-text hl-text))
         (expected-hl (oi-make-headline-from-text expected-text))
         retval
         (*org-ast-list* (list (cons "tmp" hl))))
    (oi-update id-to-update new-plist)
    (should (equal (org-element-interpret-data hl)
                   (org-element-interpret-data expected-hl))))
  (let* ((id-to-update "1")
         (new-plist (list :todo-keyword "TODO"
                          :closed nil))
         (hl-text (oi-concat-with-newlines
                   "* DONE this is the current headline"
                   "CLOSED: [2017-01-27 Fri 14:01]"
                   ":PROPERTIES:"
                   ":ID:       1"
                   ":END:"))
         (expected-text (oi-concat-with-newlines
                         "* TODO this is the current headline"
                         ":PROPERTIES:"
                         ":ID:       1"
                         ":END:"))
         (hl (oi-make-headline-from-text hl-text))
         (expected-hl (oi-make-headline-from-text expected-text))
         retval
         (*org-ast-list* (list (cons "tmp" hl))))
    (oi-update id-to-update new-plist)
    (should (equal (org-element-interpret-data hl)
                   (org-element-interpret-data expected-hl))))
  (let* ((id-to-update "1")
         (new-plist (list :paragraph "this is the new paragraph"))
         (hl-text (oi-concat-with-newlines
                   "* TODO this is the current headline"
                   ":PROPERTIES:"
                   ":ID:       1"
                   ":END:"
                   "** TODO this is the child headline"
                   ":PROPERTIES:"
                   ":ID:       A"
                   ":END:"))
         (expected-text (oi-concat-with-newlines
                         "* TODO this is the current headline"
                         ":PROPERTIES:"
                         ":ID:       1"
                         ":END:"
                         "this is the new paragraph"
                         "** TODO this is the child headline"
                         ":PROPERTIES:"
                         ":ID:       A"
                         ":END:"))
         (hl (oi-make-headline-from-text hl-text))
         (expected-hl (oi-make-headline-from-text expected-text))
         retval
         (*org-ast-list* (list (cons "tmp" hl))))
    (oi-update id-to-update new-plist)
    (should (equal (org-element-interpret-data hl)
                   (org-element-interpret-data expected-hl))))
  (let* ((id-to-update "1")
         (new-plist (list :todo-keyword "DONE"
                          :closed "2017-01-27T22:01:10.030Z"))
         (hl-text (oi-concat-with-newlines
                   "* TODO this is the current headline"
                   ":PROPERTIES:"
                   ":ID:       1"
                   ":END:"
                   "** DONE this is the child headline"
                   "CLOSED: [2017-01-26 Thu 10:00]"
                   ":PROPERTIES:"
                   ":ID:       A"
                   ":END:"))
         (expected-text (oi-concat-with-newlines
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
         (hl (oi-make-headline-from-text hl-text))
         (expected-hl (oi-make-headline-from-text expected-text))
         retval
         (*org-ast-list* (list (cons "tmp" hl))))
    (oi-update id-to-update new-plist)
    (should (equal (org-element-interpret-data hl)
                   (org-element-interpret-data expected-hl))))
  (let* ((id-to-update "1")
         (new-plist (list :todo-keyword "TODO" :closed nil))
         (hl-text (oi-concat-with-newlines
                   "* DONE this is the current headline"
                   ":PROPERTIES:"
                   ":ID:       1"
                   ":END:"
                   "** DONE this is the child headline"
                   "CLOSED: [2017-01-26 Thu 10:00]"
                   ":PROPERTIES:"
                   ":ID:       A"
                   ":END:"))
         (expected-text (oi-concat-with-newlines
                         "* TODO this is the current headline"
                         ":PROPERTIES:"
                         ":ID:       1"
                         ":END:"
                         "** DONE this is the child headline"
                         "CLOSED: [2017-01-26 Thu 10:00]"
                         ":PROPERTIES:"
                         ":ID:       A"
                         ":END:"))
         (hl (oi-make-headline-from-text hl-text))
         (expected-hl (oi-make-headline-from-text expected-text))
         retval
         (*org-ast-list* (list (cons "tmp" hl))))
    (oi-update id-to-update new-plist)
    (should (equal (org-element-interpret-data hl)
                   (org-element-interpret-data expected-hl)))))

(ert-deftest oi-move-to ()
  "Does `oi-move-to' move headlines correctly?"
  (let* ((child-id "A")
         (position 0)
         (new-parent-id "2")
         (old-parent-text (oi-concat-with-newlines
                           "* TODO this is the old parent headline"
                           "** TODO this is the child headline"
                           ":PROPERTIES:"
                           ":ID:       A"
                           ":END:"))
         (old-expected-text "* TODO this is the old parent headline")
         (new-parent-text (oi-concat-with-newlines
                           "* TODO this is the new parent headline"
                           ":PROPERTIES:"
                           ":ID:       2"
                           ":END:"))
         (new-expected-text (oi-concat-with-newlines
                             "* TODO this is the new parent headline"
                             ":PROPERTIES:"
                             ":ID:       2"
                             ":END:"
                             "** TODO this is the child headline"
                             ":PROPERTIES:"
                             ":ID:       A"
                             ":END:"))
         (old-parent-hl (oi-make-headline-from-text old-parent-text))
         (old-expected-hl (oi-make-headline-from-text old-expected-text))
         (new-parent-hl (oi-make-headline-from-text new-parent-text))
         (new-expected-hl (oi-make-headline-from-text new-expected-text))
         (*org-ast-list* (list (cons "old" old-parent-hl)
                               (cons "new" new-parent-hl))))
    (oi-move-to child-id position new-parent-id)
    (should (equal (org-element-interpret-data new-parent-hl)
                   (org-element-interpret-data new-expected-hl)))
    (should (equal (org-element-interpret-data old-parent-hl)
                   (org-element-interpret-data old-expected-hl))))
  (let* ((child-id "A")
         (position 1)
         (new-parent-id "2")
         (old-parent-text (oi-concat-with-newlines
                           "* TODO this is the old parent headline"
                           "** TODO this is the reparented headline"
                           ":PROPERTIES:"
                           ":ID:       A"
                           ":END:"))
         (old-expected-text "* TODO this is the old parent headline")
         (new-parent-text (oi-concat-with-newlines
                           "* TODO this is the new parent headline"
                           ":PROPERTIES:"
                           ":ID:       2"
                           ":END:"
                           "** TODO this is the existing child headline"
                           "*** TODO this is the existing baby headline"
                           "** TODO this is the next child headline"))
         (new-expected-text (oi-concat-with-newlines
                             "* TODO this is the new parent headline"
                             ":PROPERTIES:"
                             ":ID:       2"
                             ":END:"
                             "** TODO this is the existing child headline"
                             "*** TODO this is the existing baby headline"
                             "** TODO this is the reparented headline"
                             ":PROPERTIES:"
                             ":ID:       A"
                             ":END:"
                             "** TODO this is the next child headline"))
         (old-parent-hl (oi-make-headline-from-text old-parent-text))
         (old-expected-hl (oi-make-headline-from-text old-expected-text))
         (new-parent-hl (oi-make-headline-from-text new-parent-text))
         (new-expected-hl (oi-make-headline-from-text new-expected-text))
         (*org-ast-list* (list (cons "old" old-parent-hl)
                               (cons "new" new-parent-hl))))
    (oi-move-to child-id position new-parent-id)
    (should (equal (org-element-interpret-data new-parent-hl)
                   (org-element-interpret-data new-expected-hl)))
    (should (equal (org-element-interpret-data old-parent-hl)
                   (org-element-interpret-data old-expected-hl)))))

(ert-deftest oi-get-all-headlines ()
  "Does `oi-get-all-headlines' pull everything correctly?"
  (let* ((field-list '(:ID))
         (expected-text "[{'id': None}]")
         (hl-text "* TODO this is the only headline")
         (hl (oi-make-headline-from-text hl-text))
         (*org-ast-list* (list (cons "tmp" hl))))
    (should (equal (oi-get-all-headlines field-list)
                   expected-text)))
  (let* ((field-list '(:id))
         (expected-text "[{'id': \"A\"}]")
         (hl-text (oi-concat-with-newlines
                   "* TODO this is the only headline"
                   ":PROPERTIES:"
                   ":ID:       A"
                   ":END:"))
         (hl (oi-make-headline-from-text hl-text))
         (*org-ast-list* (list (cons "tmp" hl))))
    (should (equal (oi-get-all-headlines field-list)
                   expected-text)))
  (let* ((field-list '(:custom_id))
         (expected-text "[{'custom_id': \"A\"}]")
         (hl-text (oi-concat-with-newlines
                   "* TODO this is the only headline"
                   ":PROPERTIES:"
                   ":CUSTOM_ID: A"
                   ":END:"))
         (hl (oi-make-headline-from-text hl-text))
         (*org-ast-list* (list (cons "tmp" hl))))
    (should (equal (oi-get-all-headlines field-list)
                   expected-text)))
  (let* ((field-list '(:paragraph))
         (expected-text "[{'paragraph': \"\"}]")
         (hl-text "* TODO this is the only headline")
         (hl (oi-make-headline-from-text hl-text))
         (*org-ast-list* (list (cons "tmp" hl))))
    (should (equal (oi-get-all-headlines field-list)
                   expected-text)))
  (let* ((field-list '(:title))
         (expected-text "[{'title': \"this is the only headline\"}]")
         (hl-text "* TODO this is the only headline")
         (hl (oi-make-headline-from-text hl-text))
         (*org-ast-list* (list (cons "tmp" hl))))
    (should (equal (oi-get-all-headlines field-list)
                   expected-text)))
  (let* ((field-list '(:id))
         (expected-text "[{'id': None}, {'id': \"A\"}, {'id': \"B\"}]")
         (hl-text (oi-concat-with-newlines
                   "* TODO this is the root headline"
                   "** TODO this is the first headline"
                   ":PROPERTIES:"
                   ":ID:       A"
                   ":END:"
                   "** TODO this is the second headline"
                   ":PROPERTIES:"
                   ":ID:       B"
                   ":END:"))
         (hl (oi-make-headline-from-text hl-text))
         (*org-ast-list* (list (cons "tmp" hl))))
    (should (equal (oi-get-all-headlines field-list)
                   expected-text)))
  (let* ((field-list '(:id :parent))
         (expected-text (concat
                         "[{'id': \"A\", 'parent': None}, "
                         "{'id': \"B\", 'parent': \"A\"}]"))
         (hl-text (oi-concat-with-newlines
                   "* TODO this is the parent headline"
                   ":PROPERTIES:"
                   ":ID:       A"
                   ":END:"
                   "** TODO this is the child headline"
                   ":PROPERTIES:"
                   ":ID:       B"
                   ":END:"))
         (hl (oi-make-headline-from-text hl-text))
         (*org-ast-list* (list (cons "tmp" hl))))
    (should (equal (oi-get-all-headlines field-list)
                   expected-text)))
  (let* ((field-list '(:paragraph))
         (expected-text (concat
                         "[{'paragraph': \"A string with several"
                         "\\n\\n"
                         "newlines in it.\"}]"))
         (hl-text (oi-concat-with-newlines
                   "* TODO this is the headline"
                   "A string with several"
                   ""
                   "newlines in it."))
         (hl (oi-make-headline-from-text hl-text))
         (*org-ast-list* (list (cons "tmp" hl))))
    (should (equal (oi-get-all-headlines field-list)
                   expected-text)))
  (let* ((field-list '(:paragraph))
         (expected-text (concat
                         "[{'paragraph': \"A string with a"
                         "\\n"
                         "newline in it.\"}]"))
         (hl-text (oi-concat-with-newlines
                   "* TODO this is the headline"
                   "A string with a"
                   "newline in it."))
         (hl (oi-make-headline-from-text hl-text))
         (*org-ast-list* (list (cons "tmp" hl))))
    (should (equal (oi-get-all-headlines field-list)
                   expected-text)))
  (let* ((field-list '(:paragraph))
         (expected-text (concat
                         "[{'paragraph': \"- a bulleted list\"}]"))
         (hl-text (oi-concat-with-newlines
                   "* TODO this is the headline"
                   "- a bulleted list"))
         (hl (oi-make-headline-from-text hl-text))
         (*org-ast-list* (list (cons "tmp" hl))))
    (should (equal (oi-get-all-headlines field-list)
                   expected-text)))
  (let* ((field-list '(:todo-keyword))
         (expected-text "[{'todo_keyword': \"TODO\"}]")
         (hl-text "* TODO this is the headline")
         (hl (oi-make-headline-from-text hl-text))
         (*org-ast-list* (list (cons "tmp" hl))))
    (should (equal (oi-get-all-headlines field-list)
                   expected-text)))
  (let* ((field-list '(:id))
         (expected-text "[{'id': \"A\"}, {'id': \"B\"}]")
         (buffer-text (oi-concat-with-newlines
                       "* TODO this is a todo headline"
                       ":PROPERTIES:"
                       ":ID:       A"
                       ":END:"
                       "* DONE this is a done headline"
                       ":PROPERTIES:"
                       ":ID:       B"
                       ":END:"))
         (buffer-data (oi-make-data-from-text buffer-text))
         (*org-ast-list* (list (cons "tmp" buffer-data))))
    (should (equal (oi-get-all-headlines field-list)
                   expected-text)))
  (let* ((field-list '(:closed))
         (expected-text (concat
                         "[{'closed': None},"
                         " {'closed': \"2016-11-20T03:34:00UTC\"}]"))
         (buffer-text (oi-concat-with-newlines
                       "* TODO this headline is open"
                       "* DONE this headline is closed"
                       "CLOSED: [2016-11-19 Sat 19:34]"))
         (buffer-data (oi-make-data-from-text buffer-text))
         (*org-ast-list* (list (cons "tmp" buffer-data))))
    (should (equal (oi-get-all-headlines field-list)
                   expected-text)))
  (let* ((field-list '(:paragraph))
         (expected-text (concat
                         "[{'paragraph': \"\"},"
                         " {'paragraph': \"just the text\"}]"))
         (buffer-text (oi-concat-with-newlines
                       "* TODO this parent headline has no text"
                       "** TODO this child headline has text"
                       "just the text"))
         (buffer-data (oi-make-data-from-text buffer-text))
         (*org-ast-list* (list (cons "tmp" buffer-data))))
    (should (equal (oi-get-all-headlines field-list)
                   expected-text))))

(provide 'test-org-interaction)
;;; test-org-interaction.el ends here
