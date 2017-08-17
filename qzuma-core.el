;;; qzuma-core.el --- A core function for qzuma package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Panji Kusuma

;; Author: Panji Kusuma <epanji@gmail.com>
;; Created: 13 August 2017
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

(provide 'qzuma-core)
;;; qzuma-core.el ends here
