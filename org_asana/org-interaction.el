;;; org-interaction --- helper library for org-asana

;;; Commentary:
;; An API for manipulating Org tasks.

;;; Code:
(require 'org-agenda)
(require 'org-element)
(require 'subr-x)

(defvar *org-ast-list* ()
  "Alist of (BUF AST) for all Org Agenda buffers.")

(defun oi-get-headline-from-id (id)
  "Find the in-memory headline associated with ID."
  (let ((ast (cdr (assoc (car (org-id-find id)) *org-ast-list*)))
        (match-fn #'(lambda (hl)
                      (and (equal id (org-element-property :ID hl)) hl))))
    (org-element-map ast 'headline match-fn nil t)))

(defun oi-make-headline-from-plist (plist)
  "Create a headline from the info in PLIST.

Currently only supports :title and :paragraph."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO ")
    (insert (plist-get plist :title) "\n")
    (insert (plist-get plist :paragraph))
    (org-back-to-heading)
    (org-id-get-create)
    (org-element-at-point)))

(defun oi-reparent-1 (hl position parent-id)
  "Reparent HL to POSITION under headline with id PARENT-ID."
  (let* ((parent-hl (oi-get-headline-from-id parent-id))
         (next-sibling (nth position
                            (org-element-map parent-hl 'headline
                              #'identity))))
    (if next-sibling
        (org-element-insert-before hl next-sibling)
      (org-element-adopt-elements parent-hl hl))))

(defun oi-python-string-from-plist (plist)
  "Produce a string representation of a Python dict from PLIST."
  (with-output-to-string
    (let (comma-needed k v)
      (princ "{")
      (while plist
        (setq k (pop plist) v (pop plist))
        (when comma-needed (princ ", "))
        (princ (concat "'"
                       (substring (symbol-name k) 1)
                       "': "))
        (cond
         ((null v)
          (princ "None"))
         ((stringp v)
          (prin1 v))
         ((listp v)
          (princ (oi-python-string-from-plist v))))
        (setq comma-needed t))
      (princ "}"))))

(defun oi-get-all-headlines-1 (hl)
  "Pull a plist of info on headline HL."
  (when (eq 'todo (org-element-property :todo-type hl))
    (let* ((id (org-element-property :ID hl))
           (title (string-trim
                   (substring-no-properties
                    (car (org-element-property :title hl)))))
           (paragraph (string-trim
                       (substring-no-properties
                        (or
                         (car (org-element-contents
                               (assq 'paragraph (assq 'section hl))))
                         ""))))
           (parent (org-element-property :parent hl))
           (parent-type (car parent))
           (parent-todo-type (org-element-property :todo-type parent))
           (parent-id (and (eq parent-type 'headline)
                           (eq parent-todo-type 'todo)
                           (org-element-property :ID parent))))
      (list :id id :title title :paragraph paragraph
            :parent (when parent-id (list :id parent-id))))))

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
  (let ((new-hl (oi-make-headline-from-plist new-headline-plist)))
    (oi-reparent-1 new-hl position parent-id)
    (org-element-property :ID new-hl)))

(defun oi-delete (id)
  "Delete Org headline ID from its associated ast."
  (org-element-extract-element (oi-get-headline-from-id id)))

(defun oi-update (id plist)
  "Update Org headline ID with properties from PLIST."
  (let* ((hl (oi-get-headline-from-id id))
         k v)
    (while plist
      (setq k (pop plist) v (pop plist))
      (cond
       ((eq k :title)
        (org-element-put-property hl k v))
       ((eq k :paragraph)
        (org-element-set-contents
         (org-element-map hl 'paragraph #'identity nil t)
         v))))))

(defun oi-move-to (id position parent-id)
  "Reparent Org headline ID to POSITION under PARENT-ID."
  (let ((hl (org-element-extract-element (oi-get-headline-from-id id))))
    (oi-reparent-1 hl position parent-id)))

(defun oi-get-all-headlines ()
  "Print a Python list of dictionaries representing all Org headlines."
  (with-output-to-string
    (let (comma-needed)
      (princ "[")
      (dolist (elt *org-ast-list*)
        (dolist (plist (org-element-map (cdr elt)
                           'headline 'oi-get-all-headlines-1))
          (when comma-needed (princ ", "))
          (princ (oi-python-string-from-plist plist))
          (setq comma-needed t)))
      (princ "]"))))

(provide 'org-interaction)
;;; org-interaction.el ends here
