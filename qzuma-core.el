;;; qzuma-core.el --- A core function for qzuma package.  -*- lexical-binding: t; -*-

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

;; This file contains the core function in all qzuma package.

;;; Code:

(require 'qzuma-macro)

;; regions

(defun qz-list-from-region (start end &optional char)
  "Get list from region."
  (unless (characterp char)
    (setq char ?\n))
  (split-string
   (buffer-substring-no-properties start end)
   (make-string 1 char) t))

(defun qz-table-p (fields)
  "Make sure the first is primary key and last is data flag."
  (if (and (string-match "[\.]id$" (car fields))
           (string-match "[\.]flag$" (car (last fields))))
      t nil))

(defun qz-table-name (table-or-field)
  "Get table name from fields or primary key."
  (let ((field (cond ((listp table-or-field) (car table-or-field))
                     ((stringp table-or-field) table-or-field))))
    (string-match "^[a-z0-9_]+" field)
    (match-string 0 field)))

;; buffers

(defun qz-open-clear-buffer (name)
  "Open buffer with specific name."
  (switch-to-buffer-other-window name)
  (erase-buffer))

(defun qz-open-continue-buffer (name &optional char)
  "Open existing buffer and set point to continue."
  (unless (characterp char)
    (setq char ?\}))
  (switch-to-buffer name)
  (unless (equal 1 (goto-char (point-max)))
    (search-backward (char-to-string char) nil t 2)
    (forward-char)
    (insert (make-string 3 ?\n))
    (forward-line -1)
    (delete-blank-lines)))

;; others

(defun qz-upper-first (name)
  "Capitalize only first character from NAME."
  (concat (capitalize (substring name 0 1))
          (substring name 1)))

(defun qz-trim-field (name &optional regex)
  "Remove table name from field if exist. \
\(\"table.field\" => \"field\"\)"
  (unless regex
    (setq regex "[\.]"))
  (let ((pos (or (progn
                   (string-match regex name)
                   (match-end 0))
                 0)))
    (substring name pos)))

(defun qz-to-singular (name)
  "Change plural english NAME to singular english name if needed."
  (cond ((string-match "ies$" name)
         (concat (substring name 0 -3) "y"))
        ((string-match "[hrsx]es$" name)
         (substring name 0 -2))
        ((string-match "[ws]s$" name)
         name)
        ((string-match "s$" name)
         (substring name 0 -1))
        (t name)))

(defun qz-prefix-field (name &optional separator exception)
  "Remove '~s' '~es' in plural name and add SEPARATOR as prefix, 
only if the NAME is a key or EXCEPTION is true."
  (unless separator
    (setq separator "_"))
  (unless exception
    (setq exception nil))
  (let ((table (qz-table-name name)))
    (if (or (string-match "[\.]+[a-z]*id$" name) exception)
        (concat (qz-to-singular table) separator) "")))

(defun qz-join-field (table-1 table-2-pk)
  "Add prefix for foreign key in main table. \
\(\"table-2.key\" => \"table-1.prefix_key\"\)"
  (let ((prefix (qz-prefix-field table-2-pk))
        (field (qz-trim-field table-2-pk)))
    (format "%s.%s%s" table-1 prefix field)))

(defun qz-localize-fields (fields)
  "Localize foreign key in the fields if exists."
  (let ((table (qz-table-name fields))
        (primary-key (car fields))
        (flag (car (reverse fields))))
    (mapcar #'(lambda (f)
                (if (or (string-equal f primary-key)
                        (string-equal f flag))
                    f
                  (qz-join-field table f)))
            fields)))

(defun qz-form-field (name)
  "Change table field into form field name."
  (concat
   (qz-prefix-field name "_" t)
   (qz-trim-field name)))

(provide 'qzuma-core)
;;; qzuma-core.el ends here
