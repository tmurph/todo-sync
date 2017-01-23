;;; org-interaction --- helper library for org-asana

;;; Commentary:
;; An API for manipulating Org tasks.

;;; Code:
(require 'org-agenda)
(require 'org-element)
(require 'subr-x)

(defvar *org-ast-list* ()
  "Alist of (BUF AST) for all Org Agenda buffers.")

(defun oi-get-headline-from-id (id syntax-tree-alist)
  "Find the headline associated with ID in SYNTAX-TREE-ALIST."
  (let ((match-fn #'(lambda (hl)
                      (and (equal id (org-element-property :ID hl))
                           hl)))
        result)
    (catch 'found-it
      (dolist (elt syntax-tree-alist)
        (when (setq result
                    (org-element-map (cdr elt) 'headline match-fn nil t))
          (throw 'found-it result))))
    result))

(defun oi-make-headline-from-plist (plist)
  "Create a headline from the info in PLIST."
  (let ((special-keys '(:title :paragraph :parent))
        org-mode-hook
        k v)
    (org-element-map
        (with-temp-buffer
          (org-mode)
          (insert "* TODO ")
          (insert (plist-get plist :title) "\n")
          (insert (or (plist-get plist :paragraph) ""))
          (goto-char (point-min))
          (while plist
            (setq k (pop plist) v (pop plist))
            (unless (member k special-keys)
              (org-set-property (substring (upcase (symbol-name k)) 1) v)))
          (org-element-parse-buffer))
        'headline #'identity nil t)))

(defun oi-reparent-1 (child-hl position parent-hl)
  "Reparent CHILD-HL to POSITION under PARENT-HL."
  (let* ((parent-level (or (org-element-property :level parent-hl) 0))
         (child-level (+ parent-level 1))
         (next-sibling (nth position
                            (org-element-map
                                (org-element-contents parent-hl)
                                'headline #'identity))))
    (org-element-put-property child-hl :level child-level)
    (if next-sibling
        (org-element-insert-before child-hl next-sibling)
      (org-element-adopt-elements parent-hl child-hl))))

(defun oi-python-string-from-plist (plist)
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
            (princ (concat "'"
                           (substring (downcase (symbol-name k)) 1)
                           "': "))
            (cond
             ((null v)
              (princ "None"))
             ((stringp v)
              (prin1 v))
             ((listp v)
              (princ (oi-python-string-from-plist v))))))
        (length comma-space)))
     "}")))

(defun oi-get-title (hl)
  "Pull the title, as a string, from headline HL."
  (string-trim (substring-no-properties
                (car (org-element-property :title hl)))))

(defun oi-get-paragraph (hl)
  "Pull the paragraph contents, as a string, from headline HL."
  (string-trim
   (substring-no-properties
    (org-element-interpret-data
     (org-element-map hl 'paragraph #'identity nil t)))))

(defun oi-get-parent-id (hl)
  "Pull the parent id (or nil) from headline HL."
  (let* ((parent (org-element-property :parent hl))
         (parent-type (car parent))
         (parent-todo-type (org-element-property :todo-type parent)))
    (and (eq parent-type 'headline)
         (eq parent-todo-type 'todo)
         (org-element-property :ID parent))))

(defun oi-get-value (key hl)
  "Pull the value associated with KEY from headline HL.

This is a convenience function so I don't have to worry about
upcase / downcase issues with KEY."
  (org-element-property (intern (upcase (symbol-name key))) hl))

(defun oi-get-one-headline (fields hl)
  "Pull a plist of FIELDS info from headline HL."
  (when (eq 'todo (org-element-property :todo-type hl))
    (let (retval)
      (dolist (key fields retval)
        (push key retval)
        (cond
         ((eq key :title) (push (oi-get-title hl) retval))
         ((eq key :paragraph) (push (oi-get-paragraph hl) retval))
         ((eq key :parent) (push (oi-get-parent-id hl) retval))
         (t (push (oi-get-value key hl) retval))))
      (nreverse retval))))

(defun oi-init ()
  "Initialize abstract syntax trees for all the Org Agenda files."
  (let (lst)
    (dolist (file (org-agenda-files) lst)
      (with-current-buffer (find-file-noselect
                            (expand-file-name file org-directory))
        (org-map-entries 'org-id-get-create t 'file)
        (push (cons (abbreviate-file-name (buffer-file-name))
                    (org-element-parse-buffer))
              lst)))
    (setq *org-ast-list* (nreverse lst))))

(defun oi-insert-child (parent-id position new-headline-plist)
  "Insert under PARENT-ID at POSITION a headline from NEW-HEADLINE-PLIST.

Returns the ID of the new headline."
  (let ((parent-hl (oi-get-headline-from-id parent-id *org-ast-list*))
        (new-hl (oi-make-headline-from-plist new-headline-plist)))
    (oi-reparent-1 new-hl position parent-hl)
    (org-element-property :ID new-hl)))

(defun oi-delete (id)
  "Delete Org headline ID from its associated ast."
  (org-element-extract-element
   (oi-get-headline-from-id id *org-ast-list*)))

(defun oi-set-paragraph (hl value)
  "Set the paragraph contents of HL to VALUE."
  (org-element-set-contents
   (org-element-map hl 'paragraph #'identity nil t)
   value))

(defun oi-set-drawer (hl key value)
  "In headline HL, set or create a drawer entry like KEY: VALUE."
  (let* ((key-name (substring (upcase (symbol-name key)) 1))
         (drawer (org-element-map hl 'property-drawer #'identity nil t))
         (property (org-element-map drawer 'node-property
                     #'(lambda (np)
                         (string= key-name (org-element-property :key np)))
                     nil t)))
    (if property
        (org-element-put-property property key-name value)
      (org-element-adopt-elements
       drawer `(node-property (:key ,key-name :value ,value))))))

(defun oi-update (id plist)
  "Update Org headline ID with properties from PLIST."
  (let* ((hl (oi-get-headline-from-id id *org-ast-list*))
         k v)
    (while plist
      (setq k (pop plist) v (pop plist))
      (cond
       ((eq k :title)
        (org-element-put-property hl k v))
       ((eq k :paragraph)
        (oi-set-paragraph hl v))
       (t (oi-set-drawer hl k v))))))

(defun oi-move-to (id position parent-id)
  "Reparent Org headline ID to POSITION under PARENT-ID."
  (let ((hl (oi-get-headline-from-id id *org-ast-list*))
        (parent-hl (oi-get-headline-from-id parent-id *org-ast-list*)))
    (oi-reparent-1 (org-element-extract-element hl) position parent-hl)))

(defun oi-get-all-headlines (fields)
  "Return a string of Org headline FIELDS in Python list-of-dicts format."
  (let ((fetch-fn #'(lambda (hl) (oi-get-one-headline fields hl)))
        (comma-space ", "))
    (concat "[" (substring
                 (with-output-to-string
                   (dolist (elt *org-ast-list*)
                     (dolist (plist (org-element-map (cdr elt)
                                        'headline fetch-fn))
                       (princ comma-space)
                       (princ (oi-python-string-from-plist plist)))))
                 (length comma-space))  ; trim the leading ", "
            "]")))

(defun oi-final ()
  "Write all the Org Agenda Files from their abstract syntax trees."
  (dolist (alst *org-ast-list*)
    (let ((buf (car alst))
          (ast (cdr alst)))
      (with-current-buffer (find-file-noselect (expand-file-name buf))
        (erase-buffer)
        (insert (org-element-interpret-data ast))
        (save-buffer '(16))))))

(defun oi-repl ()
  "Start up an inferior read-eval-print-loop.

This is for using Emacs in batch mode."
  (let ((keep-going t)
        (valid-commands '(oi-init
                          oi-insert-child oi-delete oi-update oi-move-to
                          oi-get-all-headlines
                          oi-final))
        input command)
    (while keep-going
      (setq input (read) command (car-safe input))
      (cond
       ((member command valid-commands)
        (print (eval input)))
       (t (setq keep-going nil))))))

(provide 'org-interaction)
;;; org-interaction.el ends here
