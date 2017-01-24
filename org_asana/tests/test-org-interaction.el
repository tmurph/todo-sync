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


(provide 'test-org-interaction)
;;; test-org-interaction.el ends here
