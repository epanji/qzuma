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

(defun qz-tcgcia-actor-credential (fields &optional exception)
  "Get actor and credential from fields if exists.
If EXCEPTION is t, no need to localize fields."
  (unless exception
    (setq exception nil))
  (let* ((newfields (if exception fields
                      (qz-localize-fields fields)))
         (actor
          (remq nil (mapcar
                     #'(lambda (f) (when (qz-ci-actor-p f) f))
                     newfields)))
         (credential
          (remq nil (mapcar
                     #'(lambda (f) (when (qz-ci-credential-p f) f))
                     newfields))))
    (when (and actor credential)
      (append actor credential))))

(defun qz-tcgcia-form-password (credential &optional ntab neol)
  "Create form input password from credential."
  (unless ntab
    (setq ntab 0))
  (unless neol
    (setq neol 1))
  (let* ((table (qz-table-name credential))
         (name (qz-to-singular table))
         (re (format "%s.re_%s" table (qz-trim-field credential)))
         (items (list credential re)))
    (concat
     (qz-line ntab 0 "<form class=\"form-horizontal\" ")
     (qz-line 0 0 (format "name=\"form_%s\"" name))
     (qz-line 0 (+ neol 1)  " method=\"post\" action=\"\" >")
     (mapconcat
      #'(lambda (f)
          (qz-ci-form-horizontal-item f (+ ntab 1) neol))
      items (qz-line 0 1 ""))
     (qz-line 0 1 "")
     (qz-line (+ ntab 1) neol "<div class=\"form-group\">")
     (qz-line (+ ntab 2) 0 "<label class=\"control-label col-sm-2\">")
     (qz-line 0 neol "&nbsp;</label>")
     (qz-line (+ ntab 2) neol "<div  class=\"col-sm-10\">")
     (qz-line (+ ntab 3) 0 "<input class=\"btn btn-primary\" ")
     (qz-line 0 0 "type=\"submit\" name=\"btn_submit\" ")
     (qz-line 0 neol "value=\"Simpan\" />")
     (qz-line (+ ntab 2) neol "</div>")
     (qz-line (+ ntab 1) neol "</div>")
     (qz-line 0 1 "")
     (qz-line ntab neol "</form>"))))

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
  "Create password view for controller."
  (interactive)
  (if (use-region-p)
	  (let* ((fields (qz-list-from-region
                      (region-beginning)
                      (region-end)))
             (controller (downcase (read-from-minibuffer
                                    "Controller name: ")))
             (actor-credential (qz-tcgcia-actor-credential fields)))
		(if (and (qz-table-p fields) actor-credential)
			(progn
              (qz-open-clear-buffer (format "%s_password.php" controller))
              (when (fboundp 'web-mode) (web-mode))
              (insert
               (qz-line 0 2 "<?php include(\"v_dashboard_header.php\") ?>")
               (qz-tcgcia-form-password (elt actor-credential 1) 0 1)
               (qz-line 0 1 "")
               (qz-line 0 0 "<?php include(")
               (qz-line 0 1 "\"v_dashboard_footer.php\") ?>")))
		  (print "Selected region not well formatted")))
    (print "No region selected")))

(provide 'qzuma-tcgcia)
;;; qzuma-tcgcia.el ends here
