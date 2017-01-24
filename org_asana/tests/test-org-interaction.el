;;; test-org-interaction --- Unit tests for my org-interaction library

;;; Commentary:
;; Every library needs a test suite.

;;; Code:
(require 'ert)
(require 'org-interaction)

(defun oi-make-headline-from-text (text)
  "Create a headline from the Org-formatted TEXT."
  (let (org-mode-hook)
    (org-element-map (with-temp-buffer
                       (org-mode)
                       (insert text)
                       (org-element-parse-buffer))
        'headline #'identity nil t)))

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
    (should (equal retval (plist-get new-hl-plist :id)))))


(provide 'test-org-interaction)
;;; test-org-interaction.el ends here
