;;; ts-org-interaction --- helper library for todo-sync

;;; Commentary:
;; An API for manipulating Org tasks.

;;; Code:
(require 'org-agenda)
(require 'org-element)
(require 'subr-x)
(require 'parse-time)

(defvar *org-ast-list* ()
  "Alist of (BUF . AST) for all Org Agenda buffers.")

(defun ts-assoc-delete-all (key alist)
  "Delete from ALIST all elements whose car is `equal' to KEY.
Return the modified alist.
Elements of ALIST that are not conses are ignored.

Taken from `assq-delete-all' with `eq' changed to `equal'."
  (while (and (consp (car alist))
              (equal (car (car alist)) key))
    (setq alist (cdr alist)))
  (let ((tail alist) tail-cdr)
    (while (setq tail-cdr (cdr tail))
      (if (and (consp (car tail-cdr))
               (equal (car (car tail-cdr)) key))
          (setcdr tail (cdr tail-cdr))
        (setq tail tail-cdr))))
  alist)

(defun ts-find-in-headline (hl elements &optional all)
  "Return the first occurrence under HL of ELEMENTS (or a list of them ALL).

This is mainly a convenience function."
  (org-element-map (org-element-contents hl)
      elements #'identity nil (not all) '(headline plain-list)))

(defun ts-get-headline-from-id (id syntax-tree-alist)
  "Find the headline associated with ID in SYNTAX-TREE-ALIST."
  (or id (user-error "No ID provided"))
  (let ((match-fn (lambda (hl)
                    (and (equal id (org-element-property :ID hl))
                         hl)))
        result)
    (catch 'found-it
      (dolist (elt syntax-tree-alist)
        (when (setq result
                    (org-element-map (cdr elt) 'headline match-fn nil t))
          (throw 'found-it result))))
    result))

(defun ts-get-data-from-filename (filename syntax-tree-alist)
  "Find the file data associated with FILENAME in SYNTAX-TREE-ALIST."
  (or (cdr (assoc filename syntax-tree-alist))
      (cdr (assoc (concat (expand-file-name filename org-directory)
                          ".org")
                  syntax-tree-alist))))

(defun ts-make-headline-from-plist (plist)
  "Create a headline from the info in PLIST."
  (or (plist-get plist :title) (user-error "No title provided in `%s'"
                                           plist))
  (let ((special-keys '(:title :paragraph :parent :todo-keyword
                               :closed :deadline))
        text-mode-hook org-mode-hook
        k v)
    (org-element-map
        (with-temp-buffer
          (org-mode)
          (insert "* ")
          (insert (upcase (or (plist-get plist :todo-keyword) "TODO")))
          (insert " ")
          (insert (plist-get plist :title) "\n")
          (insert (or (plist-get plist :paragraph) ""))
          (goto-char (point-min))
          (when (or (plist-get plist :closed) (plist-get plist :deadline))
            (unwind-protect
                (progn
                  (advice-add 'parse-time-string :before-until
                              (lambda (string)
                                (ignore-errors
                                  (decode-time
                                   (parse-iso8601-time-string
                                    (concat string "+00:00")))))
                              '((name . "try-iso8601")))
                  (when (plist-get plist :closed)
                    (org-add-planning-info 'closed
                                           (plist-get plist :closed)))
                  (when (plist-get plist :deadline)
                    (org-add-planning-info 'deadline
                                           (plist-get plist :deadline))))
              (advice-remove 'parse-time-string "try-iso8601")))
          (while plist
            (setq k (pop plist) v (pop plist))
            (unless (or (member k special-keys) (null v))
              (org-entry-put nil (substring (upcase (symbol-name k)) 1)
                             v)))
          (org-id-get-create)
          (org-element-parse-buffer))
        'headline #'identity nil t)))

(defun ts-make-data-from-plist (plist)
  "Create an Org data structure from the info in PLIST."
  (let (text-mode-hook org-mode-hook k v)
    (with-temp-buffer
      (org-mode)
      (while plist
        (setq k (pop plist) v (pop plist))
        (insert "#+" (substring (upcase (symbol-name k)) 1))
        (insert ": " v "\n"))
      (org-element-parse-buffer))))

(defun ts-reparent-1 (child-hl left-sibling-hl parent-hl)
  "Reparent CHILD-HL after LEFT-SIBLING-HL under PARENT-HL."
  (let* ((parent-level (or (org-element-property :level parent-hl) 0))
         (child-level (+ parent-level 1))
         (all-children (ts-find-in-headline parent-hl 'headline t))
         (right-sibling-hl (cadr
                            (memq left-sibling-hl
                                  (cons nil all-children)))))
    (org-element-put-property child-hl :level child-level)
    (if right-sibling-hl
        (org-element-insert-before child-hl right-sibling-hl)
      (org-element-adopt-elements parent-hl child-hl))))

(defun ts-python-string-from-key (key)
  "Produce a string representation of a Python keyword from KEY."
  (let ((raw-string (substring (downcase (symbol-name key)) 1)))
    (while (string-match "-" raw-string)
      (setq raw-string (replace-match "_" nil nil raw-string)))
    raw-string))

(defun ts-python-string-from-plist (plist)
  "Produce a string representation of a Python dict from PLIST."
  (let ((comma-space ", ")
        (print-escape-newlines t)
        k v)
    (concat
     "{"
     (when plist
       (substring
        (with-output-to-string
          (while plist
            (setq k (pop plist) v (pop plist))
            (princ comma-space)
            (princ (concat "'" (ts-python-string-from-key k) "': "))
            (cond
             ((null v)
              (princ "None"))
             ((stringp v)
              (prin1 v))
             ((listp v)
              (princ (ts-python-string-from-plist v))))))
        (length comma-space)))
     "}")))

(defun ts-get-title (hl)
  "Pull the title, as a string, from headline HL."
  (string-trim (substring-no-properties
                (org-element-interpret-data
                 (org-element-property :title hl)))))

(defun ts-get-paragraph (hl)
  "Pull the paragraph contents, as a string, from headline HL."
  (string-trim
   (substring-no-properties
    (org-element-interpret-data
     (ts-find-in-headline hl '(plain-list paragraph src-block) t)))))

(defun ts-get-parent-id (hl)
  "Pull the parent id (or nil) from headline HL."
  (let* ((parent (org-element-property :parent hl))
         (parent-type (car parent))
         (parent-todo-type (org-element-property :todo-type parent)))
    (and (eq parent-type 'headline)
         (member parent-todo-type '(todo done))
         (org-element-property :ID parent))))

(defun ts-get-todo-keyword (hl)
  "Pull the todo keyword from headline HL."
  (substring-no-properties (org-element-property :todo-keyword hl)))

(defun ts-get-closed (hl)
  "Pull the closing timestamp from headline HL."
  (let ((closed (org-element-property :closed hl)))
    (and closed (concat (org-timestamp-format closed "%FT%T" nil t)
                        ".000Z"))))

(defun ts-get-deadline (hl)
  "Pull the deadline timestamp from headline HL."
  (let ((deadline (org-element-property :deadline hl)))
    (and deadline
         (if (org-element-property :hour-start deadline)
             (concat (org-timestamp-format deadline "%FT%T" nil t)
                     ".000Z")
           (org-timestamp-format deadline "%F")))))

(defun ts-get-filename (hl syntax-tree-alist)
  "Pull the filename associated with headline HL in SYNTAX-TREE-ALIST."
  (let ((match-fn (lambda (headline) (eq headline hl)))
        result)
    (catch 'found-it
      (dolist (elt syntax-tree-alist)
        (when (org-element-map (cdr elt) 'headline match-fn nil t)
          (setq result (car elt))
          (throw 'found-it result))))
    result))

(defun ts-get-value (key hl)
  "Pull the value associated with KEY from headline HL.

This is a convenience function so I don't have to worry about
upcase / downcase issues with KEY."
  (org-element-property (intern (upcase (symbol-name key))) hl))

(defun ts-get-one-headline (fields hl syntax-tree-alist)
  "Pull a plist of FIELDS info from headline HL in SYNTAX-TREE-ALIST."
  (when (org-element-property :todo-keyword hl)
    (let (retval)
      (dolist (key fields retval)
        (push key retval)
        (cond
         ((eq key :title) (push (ts-get-title hl) retval))
         ((eq key :paragraph) (push (ts-get-paragraph hl) retval))
         ((eq key :parent-id) (push (ts-get-parent-id hl) retval))
         ((eq key :todo-keyword) (push (ts-get-todo-keyword hl) retval))
         ((eq key :closed) (push (ts-get-closed hl) retval))
         ((eq key :deadline) (push (ts-get-deadline hl) retval))
         ((eq key :filename) (push (ts-get-filename hl syntax-tree-alist)
                                   retval))
         (t (push (ts-get-value key hl) retval))))
      (nreverse retval))))

(defun ts-get-file-value (key file-data)
  "Pull the value associated with KEY from top level of FILE-DATA."
  (let ((key-name (substring (upcase (symbol-name key)) 1)))
    (org-element-map (org-element-contents file-data)
        'keyword (lambda (kw)
                   (and (equal key-name (org-element-property :key kw))
                        (org-element-property :value kw)))
        nil t 'headline)))

(defun ts-get-one-file (fields file-data)
  "Pull a plist of FIELDS info from FILE-DATA."
  (let ((ignored-keys '(:title :paragraph :parent-id :todo-keyword
                               :closed :deadline :filename))
        v retval)
    (dolist (key fields retval)
      (and (not (memq key ignored-keys))
           (setq v (ts-get-file-value key file-data))
           (push v retval)
           (push key retval)))))

(defun ts-set-paragraph (hl value)
  "Set the paragraph contents of HL to VALUE."
  (let ((drawers (ts-find-in-headline hl 'property-drawer t))
        (new-contents (org-element-set-contents
                       (ts-find-in-headline hl 'section) value)))
    (dolist (d drawers)
      (org-element-insert-before d new-contents))))

(defun ts-set-drawer (hl key value)
  "In headline HL, set or create a drawer entry like KEY: VALUE."
  (let* ((key-name (substring (upcase (symbol-name key)) 1))
         (drawer (ts-find-in-headline hl 'property-drawer))
         (property (org-element-map drawer 'node-property
                     (lambda (np)
                       (and
                        (string= key-name (org-element-property :key np))
                        np))
                     nil t)))
    (if property
        (org-element-put-property property :value value)
      (org-element-adopt-elements
       drawer `(node-property (:key ,key-name :value ,value))))))

(defun ts-set-closed (hl value)
  "Set the CLOSED planning info of HL based on iso8601 VALUE."
  (let* ((drawer (ts-find-in-headline hl 'property-drawer))
         (planning (ts-find-in-headline hl 'planning))
         (decoded (decode-time
                   (parse-iso8601-time-string
                    (concat value "+00:00"))))
         (minute (nth 1 decoded))
         (hour (nth 2 decoded))
         (day (nth 3 decoded))
         (month (nth 4 decoded))
         (year (nth 5 decoded))
         (new-closed (list 'timestamp
                           (list :type 'inactive
                                 :year-start year
                                 :month-start month
                                 :day-start day
                                 :hour-start hour
                                 :minute-start minute))))
    (if planning
        (org-element-put-property planning :closed new-closed)
      (org-element-insert-before `(planning (:closed ,new-closed))
                                 drawer))))

(defun ts-unset-closed (hl)
  "Clear any CLOSED planning info in HL."
  (let ((planning (ts-find-in-headline hl 'planning)))
    (when planning (org-element-put-property planning :closed nil))))

(defun ts-set-deadline (hl value)
  "Set the DEADLINE planning info of HL based on VALUE.

VALUE should either be a date like YYYY-MM-DD or an iso8601 string."
  (let* ((drawer (ts-find-in-headline hl 'property-drawer))
         (planning (ts-find-in-headline hl 'planning))
         (decoded (if (string-match-p
                       "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" value)
                      (parse-time-string value)
                    (decode-time
                     (parse-iso8601-time-string
                      (concat value "+00:00")))))
         (minute (nth 1 decoded))
         (hour (nth 2 decoded))
         (day (nth 3 decoded))
         (month (nth 4 decoded))
         (year (nth 5 decoded))
         (new-deadline (list 'timestamp
                             (list :type 'active
                                   :year-start year
                                   :month-start month
                                   :day-start day
                                   :hour-start hour
                                   :minute-start minute))))
    (if planning
        (org-element-put-property planning :deadline new-deadline)
      (org-element-insert-before `(planning (:deadline ,new-deadline))
                                 drawer))))

(defun ts-unset-deadline (hl)
  "Clear any DEADLINE planning info in HL."
  (let ((planning (ts-find-in-headline hl 'planning)))
    (when planning (org-element-put-property planning :deadline nil))))

(defun ts-set-keyword (file-data key value)
  "In Org data FILE-DATA, set or create a keyword like #+KEY: VALUE."
  (let* ((key-name (substring (upcase (symbol-name key)) 1))
         (section (ts-find-in-headline file-data 'section))
         (first-hl (ts-find-in-headline file-data 'headline))
         (keyword (org-element-map section 'keyword
                    (lambda (kw)
                      (and
                       (string= key-name (org-element-property :key kw))
                       kw))
                    nil t)))
    (unless section
      (if first-hl
          (org-element-insert-before '(section ()) first-hl)
        (org-element-adopt-elements file-data '(section ())))
      (setq section (ts-find-in-headline file-data 'section)))
    (if keyword
        (org-element-put-property keyword :value value)
      (org-element-adopt-elements
       section `(keyword (:key ,key-name :value ,value))))))

(defun ts-init ()
  "Initialize abstract syntax trees for all the Org Agenda files."
  (let (lst)
    (dolist (file (org-agenda-files) lst)
      (with-current-buffer (find-file-noselect
                            (expand-file-name file org-directory))
        (org-map-entries 'org-id-get-create t 'file)
        (push (cons (buffer-file-name) (org-element-parse-buffer))
              lst)))
    (setq *org-ast-list* (nreverse lst))
    nil))

(defun ts-insert-child (parent-id left-sibling-id new-headline-plist)
  "Insert a headline under PARENT-ID after LEFT-SIBLING-ID from NEW-HEADLINE-PLIST.

Returns the ID of the new headline."
  (let ((parent-hl (ts-get-headline-from-id parent-id *org-ast-list*))
        (left-sibling-hl (and left-sibling-id
                              (ts-get-headline-from-id left-sibling-id
                                                       *org-ast-list*)))
        (new-hl (ts-make-headline-from-plist new-headline-plist)))
    (ts-reparent-1 new-hl left-sibling-hl parent-hl)
    (org-element-property :ID new-hl)))

(defun ts-insert-child-into-file (filename left-sibling-id new-headline-plist)
  "Insert a headline at the top level of FILENAME at POSITION from NEW-HEADLINE-PLIST.

Returns the ID of the new headline."
  (let ((parent-data (ts-get-data-from-filename filename *org-ast-list*))
        (left-sibling-hl (and left-sibling-id
                              (ts-get-headline-from-id left-sibling-id
                                                       *org-ast-list*)))
        (new-hl (ts-make-headline-from-plist new-headline-plist)))
    (ts-reparent-1 new-hl left-sibling-hl parent-data)
    (org-element-property :ID new-hl)))

(defun ts-insert-file (filename _ new-file-plist)
  "Create a new Org data structure associated with FILENAME from NEW-FILE-PLIST, and insert the new structure into the syntax tree alist."
  (let ((file-data (ts-make-data-from-plist new-file-plist)))
    (push (cons (expand-file-name filename org-directory)
                file-data)
          *org-ast-list*)))

(defun ts-update (id plist)
  "Update Org headline ID with properties from PLIST."
  (let ((hl (ts-get-headline-from-id id *org-ast-list*))
        k v)
    (while plist
      (setq k (pop plist) v (pop plist))
      (cond
       ((eq k :title) (org-element-put-property hl k v))
       ((eq k :paragraph) (ts-set-paragraph hl v))
       ((eq k :todo-keyword) (org-element-put-property hl k (upcase v)))
       ((eq k :closed) (if v (ts-set-closed hl v) (ts-unset-closed hl)))
       ((eq k :deadline)
        (if v (ts-set-deadline hl v) (ts-unset-deadline hl)))
       (t (ts-set-drawer hl k v))))))

(defun ts-update-file (filename plist)
  "Update the frontmatter in FILENAME with keywords from PLIST."
  (let ((file-data (ts-get-data-from-filename filename *org-ast-list*))
        k v)
    (while plist
      (setq k (pop plist) v (pop plist))
      (ts-set-keyword file-data k v))))

(defun ts-move-to (id left-sibling-id parent-id)
  "Reparent Org headline ID after LEFT-SIBLING-ID under PARENT-ID."
  (let ((hl (ts-get-headline-from-id id *org-ast-list*))
        (left-sibling-hl (and left-sibling-id
                              (ts-get-headline-from-id left-sibling-id
                                                       *org-ast-list*)))
        (parent-hl (ts-get-headline-from-id parent-id *org-ast-list*)))
    (ts-reparent-1 (org-element-extract-element hl) left-sibling-hl
                   parent-hl)))

(defun ts-move-to-file (id left-sibling-id filename)
  "Reparent Org headline ID after LEFT-SIBLING-ID at top level of FILENAME."
  (let ((hl (ts-get-headline-from-id id *org-ast-list*))
        (left-sibling-hl (and left-sibling-id
                              (ts-get-headline-from-id left-sibling-id
                                                       *org-ast-list*)))
        (file-data (ts-get-data-from-filename filename *org-ast-list*)))
    (ts-reparent-1 (org-element-extract-element hl) left-sibling-hl
                   file-data)))

(defun ts-delete (id)
  "Delete Org headline ID from its associated ast."
  (org-element-extract-element
   (ts-get-headline-from-id id *org-ast-list*)))

(defun ts-delete-file (filename)
  "Delete the data structure associated with FILENAME."
  (setq *org-ast-list* (ts-assoc-delete-all filename *org-ast-list*)))

(defun ts-get-all-headlines (fields)
  "Return a string of Org headline FIELDS in Python list-of-dicts format."
  (let ((fetch-fn (lambda (hl)
                    (ts-get-one-headline fields hl *org-ast-list*)))
        (comma-space ", "))
    (concat "[" (substring
                 (with-output-to-string
                   (dolist (elt *org-ast-list*)
                     (dolist (plist (org-element-map (cdr elt)
                                        'headline fetch-fn))
                       (princ comma-space)
                       (princ (ts-python-string-from-plist plist)))))
                 (length comma-space))  ; trim the leading ", "
            "]")))

(defun ts-get-all-filenames (fields)
  "Return a string of Org filenames in Python list-of-dicts format.

FIELDS is a plist of file-level properties to return, if they exist."
  (let ((comma-space ", "))
    (concat "[" (substring
                 (with-output-to-string
                   (dolist (elt *org-ast-list*)
                     (princ comma-space)
                     (princ (ts-python-string-from-plist
                             (append (list :id (car elt))
                                     (ts-get-one-file fields (cdr elt)))))))
                 (length comma-space))
            "]")))

(defun ts-final ()
  "Write all the Org Agenda Files from their abstract syntax trees."
  (dolist (alst *org-ast-list*)
    (let ((file (car alst))
          (ast (cdr alst)))
      (with-current-buffer (find-file-noselect
                            (expand-file-name file org-directory))
        (erase-buffer)
        (insert (org-element-interpret-data ast))
        (save-buffer '(16))))))

(defun ts-repl ()
  "Start up an inferior read-eval-print-loop.

This is for using Emacs in batch mode."
  (let ((valid-commands '(ts-init
                          ts-insert-child ts-delete ts-update ts-move-to
                          ts-insert-child-into-file ts-insert-file
                          ts-delete-file ts-update-file ts-move-to-file
                          ts-get-all-headlines ts-get-all-filenames
                          ts-final))
        input command)
    (while t
      (setq input (read) command (car-safe input))
      (cond
       ((member command valid-commands)
        (print (eval input)))
       (t (user-error "Received invalid command `%s'" command))))))

(provide 'ts-org-interaction)
;;; ts-org-interaction.el ends here
