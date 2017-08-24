;;; qzuma-tcgci.el --- A table to code generator for codeigniter.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Panji Kusuma

;; Author: Panji Kusuma <epanji@gmail.com>
;; Keywords: convenience, qzuma

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Generate full application for codeigniter from table.

;;; Code:

(require 'qzuma-ci-core)

;; functions

(defun qz-tcgci-method (fields name &optional class type)
  "Create function from table fields inside class."
  (if (qz-table-p fields)
      (progn
        (unless class
          (setq class "Debug"))
        (unless type
          (setq type "global"))
        (qz-open-continue-buffer (concat class ".php"))
        (when (equal 1 (point-at-bol))
          (insert (qz-ci-class-wrapper name type))
          (forward-line -1))
        (when (fboundp 'web-mode) (web-mode))
        (insert

         (qz-line 0 1 "")
         (qz-line 1 1 (format "public function %s() {" name))

         (qz-tcgci-method-contents fields name 2)

         (qz-ci-method-footer type class name)
         (qz-line 1 0 "}")))
    (print "Selected region not well formatted")))

(defun qz-tcgci-method-contents (fields name &optional ntab neol)
  "Create contents from fields."
  (unless ntab
    (setq ntab 0))
  (unless neol
    (setq neol 1))
  (let ((table (qz-table-name fields)))
    (cond
     ((string-equal name "create")
      (concat
       (qz-line ntab 0 (format "$data_%s" table))
       (qz-line 0 2" = array();")
       (qz-ci-data-post fields ntab neol)
       (qz-line 0 1 "")
       (qz-line ntab 0 "$this->db->insert(")
       (qz-line 0 0 (format "'%s', " table))
       (qz-line 0 neol (format "$data_%s);" table))
       (qz-line ntab neol "return $this->db->insert_id();")))
     ((string-equal name "read")
      (concat "model content read"))
     ((string-equal name "update")
      (concat "model content update"))
     ((string-equal name "delete")
      (concat "model content delete")))))

(defun qz-tcgci-upload-exists (fields &optional ntab neol)
  "Add upload function for the type if exists."
  (unless ntab
    (setq ntab 0))
  (unless neol
    (setq neol 1))
  (let ((uploads (qz-ci-upload-exists fields)))
    (when uploads
      (concat
       (qz-line 0 1 "")
       (qz-line 0 1 (qz-ci-define-private-upload ntab neol))
       (mapconcat
        #'(lambda (type)
            (qz-ci-define-function-upload type ntab neol))
        uploads (qz-line 0 1 ""))))))

;; commands

(defun qz-tcgci-create-controller ()
  "Create controller from table fields in region."
  (interactive)
  (if (use-region-p)
	  (let ((fields (qz-list-from-region
                     (region-beginning)
                     (region-end)))
            (type "controller")
			(controller (qz-upper-first (read-from-minibuffer
                                         "Controller name: "))))
		(if (qz-table-p fields)
			(progn
              (qz-open-clear-buffer (format "%s.php" controller))
              (insert (qz-ci-class-wrapper controller type))
              (qz-open-continue-buffer (concat controller ".php"))
              (qz-tcgci-method fields "index" controller type)
              ;; ok
              )
		  (print "Selected region not well formatted")))
	(print "No region selected")))

(defun qz-tcgci-create-model ()
  "Create model from table fields in region."
  (interactive)
  (if (use-region-p)
	  (let* ((fields (qz-list-from-region
                      (region-beginning)
                      (region-end)))
             (type "model")
             (model (qz-upper-first
                     (qz-to-singular
                      (qz-table-name fields)))))
		(if (qz-table-p fields)
			(progn
              (qz-open-clear-buffer (format "%s_model.php" model))
              (insert (qz-ci-class-wrapper model type))
              (qz-open-continue-buffer (format "%s_model.php" model))
              (insert (qz-tcgci-upload-exists fields 1 1))
              (qz-tcgci-method fields "create" (format "%s_model" model))
              ;; ok
              )
		  (print "Selected region not well formatted")))
	(print "No region selected")))

(provide 'qzuma-tcgci)
;;; qzuma-tcgci.el ends here
