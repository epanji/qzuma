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



(qz-define-field-type
 "qz-ci" "picture" "type=\"file\""

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
 "qz-ci" "audio" "type=\"file\""

 ;; english

 "audio[s]?$"
 "music[s]?$"
 "sound[s]?$"

 ;; indonesia

 "lagu$"
 "musik$"
 "suara$")

(qz-define-field-type
 "qz-ci" "video" "type=\"file\""

 ;; english

 "clip[s]?$"
 "movie[s]?$"
 "video[s]?$"

 ;; indonesia

 "film$"
 "klip$"
 "vidio$")

(qz-define-field-type
 "qz-ci" "file" "type=\"file\""

 ;; english

 "file[s]?$"
 "image[s]?$"

 ;; indonesia

 "berkas$")

(setq qz-ci-file
      (append qz-ci-file
              qz-ci-picture
              qz-ci-audio
              qz-ci-video))



(defun qz-ci-define-private-upload (&optional ntab neol)
  "Create private function upload for model."
  (unless ntab
    (setq ntab 0))
  (unless neol
    (setq neol 1))
  (concat
   (qz-line ntab 0 "private function _do_upload")
   (qz-line 0 neol "($config, $name, $table = '') {")
   (qz-line (+ ntab 1) neol "$filename = '';")
   (qz-line (+ ntab 1) neol "$this->upload->initialize($config);")
   (qz-line (+ ntab 1) neol "if (isset($_FILES[$name]['name'])) {")
   (qz-line (+ ntab 2) neol "if ($this->upload->do_upload($name)) {")
   (qz-line (+ ntab 3) neol "$data = $this->upload->data();")
   (qz-line (+ ntab 3) neol "if ($table != '') {")
   (qz-line (+ ntab 4) neol "$filename = $data['file_name'];")
   (qz-line (+ ntab 3) neol "}")
   (qz-line (+ ntab 2) neol "}")
   (qz-line (+ ntab 1) neol "}")
   (qz-line (+ ntab 1) neol "return $filename;")
   (qz-line ntab neol "}")))

(defun qz-ci-define-function-upload (name &optional ntab neol)
  "Create upload function depends on name."
  (unless ntab
    (setq ntab 0))
  (unless neol
    (setq neol 1))
  (cond
   ((string-equal name "picture")
    (concat
     (qz-line ntab 0 "public function do_upload_picture")
     (qz-line 0 neol "($name, $table = '') {")
     (qz-line (+ ntab 1) neol "$config['encrypt_name'] = true;")
     (qz-line (+ ntab 1) neol "$config['upload_path'] = './uploads/';")
     (qz-line (+ ntab 1) neol "$config['allowed_types'] = 'gif|jpg|png';")
     (qz-line (+ ntab 1) neol "$config['max_size'] = '10000';")
     (qz-line (+ ntab 1) neol "$config['max_width'] = '10240';")
     (qz-line (+ ntab 1) (+ neol 1) "$config['max_height'] = '7680';")
     (qz-line (+ ntab 1) 0 "return $this->_do_upload")
     (qz-line 0 neol "($config, $name, $table);")
     (qz-line ntab neol "}")))
   ((string-equal name "audio")
    (concat
     (qz-line ntab 0 "public function do_upload_audio")
     (qz-line 0 neol "($name, $table = '') {")
     (qz-line (+ ntab 1) neol "$config['encrypt_name'] = true;")
     (qz-line (+ ntab 1) neol "$config['upload_path'] = './uploads/';")
     (qz-line (+ ntab 1) neol "$config['allowed_types'] = 'mp3';")
     (qz-line (+ ntab 1) (+ neol 1) "$config['max_size'] = '100000';")
     (qz-line (+ ntab 1) 0 "return $this->_do_upload")
     (qz-line 0 neol "($config, $name, $table);")
     (qz-line ntab neol "}")))
   ((string-equal name "video")
    (concat
     (qz-line ntab 0 "public function do_upload_video")
     (qz-line 0 neol "($name, $table = '') {")
     (qz-line (+ ntab 1) neol "$config['encrypt_name'] = true;")
     (qz-line (+ ntab 1) neol "$config['upload_path'] = './uploads/';")
     (qz-line (+ ntab 1) neol "$config['allowed_types'] = 'mp4';")
     (qz-line (+ ntab 1) (+ neol 1) "$config['max_size'] = '100000';")
     (qz-line (+ ntab 1) 0 "return $this->_do_upload")
     (qz-line 0 neol "($config, $name, $table);")
     (qz-line ntab neol "}")))
   ((string-equal name "file")
    (concat
     (qz-line ntab 0 "public function do_upload_file")
     (qz-line 0 neol "($name, $table = '') {")
     (qz-line (+ ntab 1) neol "$config['encrypt_name'] = true;")
     (qz-line (+ ntab 1) neol "$config['upload_path'] = './uploads/';")
     (qz-line (+ ntab 1) neol "$config['allowed_types'] = 'pdf|doc|docx';")
     (qz-line (+ ntab 1) (+ neol 1) "$config['max_size'] = '100000';")
     (qz-line (+ ntab 1) 0 "return $this->_do_upload")
     (qz-line 0 neol "($config, $name, $table);")
     (qz-line ntab neol "}")))))

(defun qz-ci-class-wrapper (name &optional type)
  "Decide to return type class wrapper. \
\(model, controller, default\)"
  (unless type
    (setq type "global"))
  (cond
   ((string-equal type "model")
    (concat
     (qz-line 0 1 "<?php")
     (qz-line 0 0 "defined('BASEPATH') OR ")
     (qz-line 0 2 "exit('No direct script access allowed');")
     (qz-line 0 0 (format "class %s_model " name))
     (qz-line 0 1 "extends CI_Model")
     (qz-line 0 1 "{")
     (qz-line 1 1 "public function __construct() {")
     (qz-line 2 1 "parent::__construct();")
     (qz-line 2 1 "$this->load->library('pagination');")
     (qz-line 2 1 "$this->load->library('form_validation');")
     (qz-line 2 1 "$this->load->library('upload');")
     (qz-line 1 2 "}")
     (qz-line 0 0 "}")))
   ((string-equal type "controller")
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
   (t ;; global class wrapper
    (concat
     (qz-line 0 1 "<?php")
     (qz-line 0 0 "defined('BASEPATH') OR ")
     (qz-line 0 2 "exit('No direct script access allowed');")
     (qz-line 0 0 (format "class %s " name))
     (qz-line 0 1 "{")
     (qz-line 1 2 "public function __construct() {")
     (qz-line 1 2 "}")
     (qz-line 0 0 "}")))))

(defun qz-ci-method-footer (&optional type class name)
  "Decide to which view for method. \
\(controller, service\)"
  (unless type
    (setq type ""))
  (unless class
    (setq class "class"))
  (unless name
    (setq name "function"))
  (cond
   ((string-equal type "controller")
    (concat
     (qz-line 2 0 "$this->load->view('")
     (qz-line 0 0 (downcase (concat class "_" name)))
     (qz-line 0 1 "', $data);")))
   ((string-equal type "service")
    (concat
     (qz-line 0 1 "")
     (qz-line 2 1 "header('Content-Type: application/json');")
     (qz-line 2 1 "echo json_encode($data);")))
   (t "")))



(defun qz-ci-upload-exists (fields)
  "Get list file type if exists"
  (remq
   nil
   (mapcar
    #'(lambda (f)
        (cond
         ((qz-ci-picture-p f) "picture")
         ((qz-ci-audio-p f) "audio")
         ((qz-ci-video-p f) "video")
         ((qz-ci-file-p f) "file")
         (t nil)))
    fields)))

(provide 'qzuma-ci-core)
;;; qzuma-ci-core.el ends here
