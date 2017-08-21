;;; qzuma-ci-core.el --- A core function specific for codeigniter.  -*- lexical-binding: t; -*-

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

;; A core function used in all tcgci* files.

;;; Code:

(require 'qzuma-core)

(defun qz-ci-post (field)
  "Create post format from field."
  (format "$this->input->post('%s')"
          (qz-form-field field)))

(defun qz-ci-line-post (field)
  "Create one line post format from field."
  (format "$data_%s['%s'] = %s;"
          (qz-table-name field)
          (qz-trim-field field)
          (qz-ci-post field)))

(defun qz-ci-data-post (fields &optional ntab neol exception)
  "Create multiple line post format from fields."
  (unless ntab
    (setq ntab 0))
  (unless neol
    (setq neol 1))
  (unless exception
    (setq exception nil))
  (if (or (qz-table-p fields) exception)
      (let ((newfields (if exception
                           fields
                         (cdr (butlast (qz-localize-fields fields))))))
        (mapconcat
         #'(lambda (f)
             (qz-line ntab neol (qz-ci-line-post f)))
         newfields "")) ""))

(defun qz-ci-line-validation (field)
  "Create one line validation from field."
  (concat "$this->form_validation->set_rules"
          (format "('%s', '%s', 'required');"
                  (qz-form-field field)
                  (qz-trim-field field))))

(defun qz-ci-data-validation (fields &optional ntab neol exception)
  "Create multiple line validation format from fields."
  (unless ntab
    (setq ntab 0))
  (unless neol
    (setq neol 1))
  (unless exception
    (setq exception nil))
  (if (or (qz-table-p fields) exception)
      (let ((newfields (if exception
                           fields
                         (cdr (butlast (qz-localize-fields fields))))))
        (mapconcat
         #'(lambda (f)
             (qz-line ntab neol (qz-ci-line-validation f)))
         newfields "")) ""))

(defun qz-ci-query-select (field &optional table)
  "Create query string from field."
  (format "%s AS %s" field
          (if (string-equal table (qz-table-name field))
              (qz-trim-field field)
            (qz-form-field field))))

(defun qz-ci-query-select-line (field &optional operator separator table)
  "Create query string line from field."
  (unless operator
    (setq operator ".="))
  (unless separator
    (setq separator ""))
  (format "$select %s \"%s%s \";"
          operator
          (qz-ci-query-select field table)
          separator))

(defun qz-ci-query-select-multi-line (fields &optional ntab neol exception)
  "Create multi line query string from fields.
EXCEPTION could be the name of table as string or just symbol t."
  (unless ntab
    (setq ntab 0))
  (unless neol
    (setq neol 1))
  (unless exception
    (setq exception nil))
  (if (or (qz-table-p fields) exception)
      (let* ((table (or exception
                        (qz-table-name fields)))
             (newfields (if exception
                            fields
                          (butlast (qz-localize-fields fields))))
             (st (car newfields))
             (lst (car (reverse newfields))))
        (mapconcat
         #'(lambda (f)
             (let ((op (if (string-equal f st) " =" ".="))
                   (sp (if (string-equal f lst) "" ",")))
               (qz-line ntab neol (qz-ci-query-select-line f op sp table))))
         newfields "")) ""))

(provide 'qzuma-ci-core)
;;; qzuma-ci-core.el ends here
