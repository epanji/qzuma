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
  "Create login view to perform login."
  (interactive)
  (if (use-region-p)
	  (let* ((fields (qz-list-from-region
                      (region-beginning)
                      (region-end)))
             (controller (downcase (read-from-minibuffer
                                    "Controller name: ")))
             (table (qz-table-name fields))
             (actor-credential (qz-tcgcia-actor-credential fields))
             (actor (elt actor-credential 0))
             (credential (elt actor-credential 1)))
		(if (and (qz-table-p fields) actor-credential)
			(progn
              (qz-open-clear-buffer (format "%s_index.php" controller))
              (when (fboundp 'web-mode) (web-mode))
              (insert
               (qz-line 0 1 "<!DOCTYPE html>")
               (qz-line 0 1 "<html lang=\"en\">")
               (qz-line 1 1 "<head>")
               (qz-line 2 1 "<meta charset=\"utf-8\">")
               (qz-line 2 1 "<meta http-equiv=\"X-UA-Compatible\" "
                        "content=\"IE=edge\">")
               (qz-line 2 1 "<meta name=\"viewport\" content="
                        "\"width=device-width, initial-scale=1\">")
               (qz-line 2 1 "<link rel=\"icon\" href=\"<?php echo "
                        "base_url();?>assets/favicon.ico\">")
               (qz-line 2 1 "<title>Halaman Login</title>")
               (qz-line 2 1 "<link href=\"<?php echo base_url();?>"
                        "assets/bootstrap/css/bootstrap.min.css\" rel="
                        "\"stylesheet\">")
               (qz-line 2 1 "<style>")
               (qz-line 3 1 "body, html { height: 100%; background-"
                        "repeat: no-repeat; background-image: linear"
                        "-gradient(rgb(104, 145, 162), "
                        "rgb(12, 97, 33)); }")
               (qz-line 3 1 ".card-container.card { max-width: 350px; "
                        "padding: 40px 40px; }")
               (qz-line 3 1 ".btn { font-weight: 700; height: 36px; "
                        "-moz-user-select: none; -webkit-user-select: "
                        "none; user-select: none; cursor: default; }")
               (qz-line 3 1 ".card { background-color: #F7F7F7; padding:"
                        " 20px 25px 30px; margin: 0 auto 25px; margin-"
                        "top: 50px; -moz-border-radius: 2px; -webkit-"
                        "border-radius: 2px; border-radius: 2px; -moz-"
                        "box-shadow: 0px 2px 2px rgba(0, 0, 0, 0.3); "
                        "-webkit-box-shadow: 0px 2px 2px rgba(0, 0, 0, "
                        "0.3); box-shadow: 0px 2px 2px "
                        "rgba(0, 0, 0, 0.3); }")
               (qz-line 3 1 ".profile-img-card { width: 200px; height: "
                        "200px; margin: 0 auto 20px; display: block; "
                        "-moz-border-radius: 50%; -webkit-border-"
                        "radius: 50%; }")
               (qz-line 3 1 ".form-signin input[type=password], .form-"
                        "signin input[type=text], .form-signin button "
                        "{ width: 100%; display: block; margin-bottom: "
                        "10px; font-size: 16px; height: 44px; z-index: "
                        "1; position: relative; -moz-box-sizing: "
                        "border-box; -webkit-box-sizing: border-box; "
                        "box-sizing: border-box; }")
               (qz-line 3 1 ".form-signin .form-control:focus { border-"
                        "color: rgb(104, 145, 162); outline: 0; -webkit"
                        "-box-shadow: inset 0 1px 1px rgba(0,0,0,.075),"
                        "0 0 8px rgb(104, 145, 162); box-shadow: "
                        "inset 0 1px 1px rgba(0,0,0,.075),0 0 8px "
                        "rgb(104, 145, 162); }")
               (qz-line 3 1 ".btn.btn-signin { background-color: "
                        "rgb(104, 145, 162); padding: 0px; font-weight: "
                        "700; font-size: 14px; height: 38px; -moz-border"
                        "-radius: 3px; -webkit-border-radius: 3px; "
                        "border-radius: 3px; border: none; "
                        "-o-transition: all 0.218s; -moz-transition: "
                        "all 0.218s; -webkit-transition: "
                        "all 0.218s; transition: all 0.218s; }")
               (qz-line 3 1 ".btn.btn-signin:hover, .btn.btn-signin:"
                        "active, .btn.btn-signin:focus { "
                        "background-color: rgb(12, 97, 33); }")
               (qz-line 2 1 "</style>")
               (qz-line 1 1 "</head>")
               (qz-line 1 1 "<body>")
               (qz-line 2 1 "<div class=\"container\">")
               (qz-line 3 1 "<div class=\"card card-container\">")
               (qz-line 4 1 "<img class=\"profile-img-card\" "
                        "src=\"<?php echo base_url();?>assets/"
                        "image/logo.png\" />")
               (qz-line 4 1 "<form name=\"form_login\" method=\"post\" "
                        "action=\"\" class=\"form-signin\">")
               (qz-line 5 1 "<input type=\"text\" name="
                        (format "\"login_%s\" " (qz-trim-field actor))
                        "class=\"form-control\" placeholder="
                        (format
                         "\"%s\" autofocus>"
                         (qz-upper-first (qz-trim-field actor))))
               (qz-line 5 1 "<?php echo form_error"
                        (format "('login_%s');?>" (qz-trim-field actor)))
               (qz-line 5 1 "<input type=\"password\" name="
                        (format "\"login_%s\" " (qz-trim-field credential))
                        "class=\"form-control\" placeholder="
                        (format
                         "\"%s\">"
                         (qz-upper-first (qz-trim-field credential))))
               (qz-line 5 1 "<?php echo form_error"
                        (format "('login_%s');?>"
                                (qz-trim-field credential)))
               (qz-line 5 1 "<button name=\"btn_login\" "
                        "class=\"btn btn-lg btn-primary btn-block "
                        "btn-signin\" type=\"submit\" value="
                        "\"Login\">Login</button>")
               (qz-line 5 1 "<?php if ($pesan = @$this->session->"
                        "flashdata('pesan')) { ?>")
               (qz-line 6 1 "<span class=\"help-block error\"><?php "
                        "echo $pesan;?></span>")
               (qz-line 5 1 "<?php } ?>")
               (qz-line 4 1 "</form>")
               (qz-line 3 1 "</div>")
               (qz-line 2 1 "</div>")
               (qz-line 1 1 "</body>")
               (qz-line 0 1 "</html>")))
          (print "Selected region not well formatted")))
    (print "No region selected")))

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
