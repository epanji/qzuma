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

(defun qz-prefix-field (name &optional separator)
  "Remove '~s' '~es' in plural name and add SEPARATOR as prefix, 
only if the NAME is a key."
  (unless separator
    (setq separator "_"))
  (let ((table (qz-table-name name)))
    (if (string-match "[\.]+[a-z]*id$" name)
        (cond ((string-match "ies$" table)
               (concat (substring table 0 -3) "y" separator))
              ((string-match "[hrsx]es$" table)
               (concat (substring table 0 -2) separator))
              ((string-match "[ws]s$" table)
               (concat table separator))
              ((string-match "s$" table)
               (concat (concat (substring table 0 -1) separator)))
              (t (concat table separator))) "")))

(defun qz-join-field (table1 table2key)
  "Add prefix for foreign key in main table. \
\(\"table2.key\" => \"table1.prefix_key\"\)"
  (let ((prefix (qz-prefix-field table2key))
        (field (qz-trim-field table2key)))
    (format "%s.%s%s" table1 prefix field)))

(provide 'qzuma-core)
;;; qzuma-core.el ends here
