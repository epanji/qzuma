;;; qzuma-tcgcia.el --- A table to code generator for authentication in codeigniter.  -*- lexical-binding: t; -*-

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

;; Generate authentication related file in codeigniter using table fields.

;;; Code:

(require 'qzuma-ci-core)

(qz-define-field-type
 "qz-tcgcia" "actor" "User"
 "username$"
 "email$"
 "nip$"
 "nim$")

(qz-define-field-type
 "qz-tcgcia" "credential" "Credential"
 "password$"
 "passwd$")



(defun qz-tcgcia-actor-credential (fields &optional exception)
  "Exclude other fields beside actor and credential."
  (unless exception
    (setq exception nil))
  (remq
   nil
   (mapcar
    #'(lambda (f)
        (when (or (qz-tcgcia-actor-p f)
                  (qz-tcgcia-credential-p f))
          f))
    (if exception
        fields
      (qz-localize-fields fields)))))

;;; commands

(defun qz-tcgcia-create-controller ()
  "")

(defun qz-tcgcia-create-view-index ()
  "")

(defun qz-tcgcia-create-view-home ()
  "Create home view as redirection target after login."
  (interactive)
  (let ((controller (downcase (read-from-minibuffer "Controller name: "))))
    (qz-open-clear-buffer (format "%s_home.php" controller))
    (when (fboundp 'web-mode) (web-mode))
    (insert
     (qz-line 0 2 "<?php include(\"v_dashboard_header.php\") ?>")
     (qz-ci-flash-message 0 1 "pesan")
     (qz-line 0 1 "")
     (qz-line 0 2 "<h3>Selamat datang.</h3>")
     (qz-line 0 0 "<?php include(")
     (qz-line 0 1 "\"v_dashboard_footer.php\") ?>"))))

(defun qz-tcgcia-create-view-password ()
  "")

(provide 'qzuma-tcgcia)
;;; qzuma-tcgcia.el ends here
