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

(require 'qzuma-core)

(qz-define-field-type
 "qz-tcgci" "picture" "type=\"file\""

 ;; english

 "icon[s]?$"
 "image[s]?$"
 "logo[s]?$"
 "photo[s]?$"
 "picture[s]?$"

 ;; indonesia

 "foto$"
 "gambar$"
 "ikon$")

(qz-define-field-type
 "qz-tcgci" "audio" "type=\"file\""

 ;; english

 "audio[s]?$"
 "music[s]?$"
 "sound[s]?$"

 ;; indonesia

 "lagu$"
 "musik$"
 "suara$")

(qz-define-field-type
 "qz-tcgci" "video" "type=\"file\""

 ;; english

 "clip[s]?$"
 "movie[s]?$"
 "video[s]?$"

 ;; indonesia

 "film$"
 "klip$"
 "vidio$")

(qz-define-field-type
 "qz-tcgci" "file" "type=\"file\""

 ;; english

 "file[s]?$"
 "image[s]?$"

 ;; indonesia

 "berkas$")

(setq qz-tcgci-file
      (append qz-tcgci-file
              qz-tcgci-picture
              qz-tcgci-audio
              qz-tcgci-video))

;; functions

(defun qz-tcgci-class-controller (name)
  "Create class controller."
  (concat
   (qz-line 0 1 "<?php")
   (qz-line 0 0 "defined('BASEPATH') OR ")
   (qz-line 0 2 "exit('No direct script access allowed');")
   (qz-line 0 0 (format "class %s " name))
   (qz-line 0 1 "extends CI_Controller")
   (qz-line 0 1 "{")
   (qz-line 1 1 "public function __construct() {")
   (qz-line 2 1 "parent::__construct();")
   (qz-line 1 2 "}")
   (qz-line 0 0 "}")))

(defun qz-tcgci-function-controller (fields name &optional controller)
  "Create function from table fields inside controller."
  (if (qz-table-p fields)
      (progn
        (unless controller
          (setq controller "Controller"))
        (qz-open-continue-buffer (concat controller ".php"))
        (when (equal 1 (point-at-bol))
          (insert (qz-tcgci-class-controller controller))
          (forward-line -1))
        (when (fboundp 'web-mode) (web-mode))
        (insert
         (qz-line 0 1 "")
         (qz-line 1 1 (format "public function %s() {" name))
         (qz-line 2 2 "$data = array();")

         ;; content get / set or if-get
         ;; (qz-function-contents fields 2)

         (qz-line 0 1 "")
         (qz-line 2 0 "$this->load->view('")
         (qz-line 0 0 (downcase (concat controller "_" name)))
         (qz-line 0 1 "', $data);")
         (qz-line 1 0 "}")))
    (print "Selected region not well formatted")))

;; commands

(defun qz-tcgci-create-controller ()
  "Create controller from table fields in region."
  (interactive)
  (if (use-region-p)
	  (let ((fields (qz-list-from-region
                     (region-beginning)
                     (region-end)))
			(controller (qz-upper-first
                         (read-from-minibuffer "Controller name: "))))
		(if (qz-table-p fields)
			(progn
              (qz-open-clear-buffer (concat controller ".php"))
              (insert (qz-tcgci-class-controller controller))
              (qz-open-continue-buffer (concat controller ".php"))
              (qz-tcgci-function-controller fields "index" controller)
              (qz-tcgci-function-controller fields "data" controller)

              ;; ok
              )
		  (print "Selected region not well formatted")))
	(print "No region selected")))

(provide 'qzuma-tcgci)
;;; qzuma-tcgci.el ends here
