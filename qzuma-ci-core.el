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

(defun qz-ci-file-post (field &optional ntab neol)
  "Create one file post format from field."
  (unless ntab
    (setq ntab 0))
  (unless neol
    (setq neol 1))
  (let ((table (qz-table-name field))
        (form-name (qz-form-field field))
        (field-name (qz-trim-field field))
        (fname (cond ((qz-ci-picture-p field) "picture")
                     ((qz-ci-audio-p field) "audio")
                     ((qz-ci-video-p field) "video")
                     ((qz-ci-file-p field) "file")
                     (t nil))))
    (when fname
      (concat
       (qz-line ntab 0 "if (($filename = $this->do_upload_")
       (qz-line 0 0 (format "%s('%s'," fname form-name))
       (qz-line 0 neol (format " '%s')) != '') {" field-name))
       (qz-line (+ ntab 1) 0 (format "$data_%s" table))
       (qz-line 0 0 (format "['%s']" field-name))
       (qz-line 0 neol " = $filename;")
       (qz-line ntab neol"}")))))

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
                         (cdr (butlast
                               (qz-localize-fields
                                fields t)))))
            files)
        (concat
         (mapconcat
          #'(lambda (f)
              (if (qz-ci-file-p f)
                  (prog1 "" (setq files (cons f files)))
                (qz-line ntab neol (qz-ci-line-post f))))
          newfields "")
         (if files
             (mapconcat
              #'(lambda (f)
                  (qz-ci-file-post f ntab neol))
              fields "")
           ""))) ""))

(defun qz-ci-line-validation (field &optional commented)
  "Create one line validation from field."
  (concat (if commented "#" "")
          "$this->form_validation->set_rules"
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
                         (cdr (butlast
                               (qz-localize-fields
                                fields t))))))
        (mapconcat
         #'(lambda (f)
             (qz-line ntab neol (qz-ci-line-validation f (qz-ci-file-p f))))
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

(defun qz-ci-line-join (table field)
  "Create line join codeigniter format."
  (concat
   "$this->db->join"
   (format "('%s'" (qz-table-name field))
   (format ", '%s " field)
   (format "= %s');" (qz-join-field table field))))

(defun qz-ci-data-join (fields &optional ntab neol exception)
  "Create multiple line query join if exists.
EXCEPTION must be the name of table as string or nil."
  (unless ntab
    (setq ntab 0))
  (unless neol
    (setq neol 1))
  (unless exception
    (setq exception nil))
  (let ((table (or exception
                   (qz-table-name fields)))
        (joins (remq nil (mapcar #'(lambda (f)
                                     (when (qz-identity-p f) f))
                                 (if exception
                                     fields
                                   (cdr (butlast fields)))))))
    (if joins
        (mapconcat
         #'(lambda (f)
             (qz-line ntab neol (qz-ci-line-join table f)))
         joins "")
      "")))



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

(qz-define-field-type
 "qz-ci" "textarea" ""

 ;; english
 "content$"
 "description$"
 "text$"

 ;; indonesia
 "isi$"
 "konten$"
 "deskripsi$"
 "keterangan$")



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

(defun qz-ci-class-wrapper (name &optional type table)
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
     (qz-line 2 0 "$this->load->model")
     (qz-line 0 1 (format "('%s_model');" (qz-to-singular table)))
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



(defun qz-ci-view-form-search (name)
  "Form search for name as model name."
  (concat
   (qz-line 0 1 "<form name=\"form_search\" method=\"get\" action=\"\" >")
   (qz-line 1 1 "<div class=\"input-group col-sm-4 col-sm-offset-8\">")
   (qz-line 2 0 "<input class=\"form-control\" placeholder=\"Search\" ")
   (qz-line 0 0 "type=\"text\" name=")
   (qz-line 0 1 (format "\"%s_search\" value=\"\" />" name))
   (qz-line 2 1 "<span class=\"input-group-btn\">")
   (qz-line 3 0 "<button class=\"btn btn-default\" type=\"submit\">")
   (qz-line 0 0 "<span class=\"glyphicon glyphicon-search\" ")
   (qz-line 0 1 "aria-hidden=\"true\"></span></button>")
   (qz-line 2 1 "</span>")
   (qz-line 1 1 "</div>")
   (qz-line 0 1 "</form>")))

(defun qz-ci-form-horizontal-item (name &optional ntab neol)
  "Create bootstrap form horizontal."
  (unless ntab
    (setq ntab 0))
  (unless neol
    (setq neol 1))
  (let ((input-type (cond ((qz-ci-file-p name) qz-ci-file-alias)
                          ((qz-ci-textarea-p name) qz-ci-textarea-alias)
                          (t "type=\"text\"")))
        (input-class (cond ((qz-ci-file-p name) "btn btn-default")
                           ((qz-ci-textarea-p name)
                            "form-control ckeditor\" rows=\"10")
                           (t "form-control")))
        (input-tag (if (qz-ci-textarea-p name) "textarea" "input"))
        (value-open (if (qz-ci-textarea-p name) "value=\"\">" "value=\""))
        (value-close (if (qz-ci-textarea-p name) "</textarea>" "\" />")))
    (concat
     (qz-line ntab neol "<div class=\"form-group\">")
     (qz-line (+ ntab 1) 0 "<label class=\"control-label col-sm-2\">")
     (qz-line 0 0 (format "%s" (qz-upper-first (qz-trim-field name))))
     (qz-line 0 neol "</label>")
     (qz-line (+ ntab 1) neol "<div  class=\"col-sm-10\">")
     (if (qz-key-p name)
         (concat
          (qz-line (+ ntab 2) 0 "<?php echo form_dropdown")
          (qz-line 0 0 (format "('%s', " (qz-form-field name)))
          (qz-line 0 0 (format "$%sArray" (qz-to-singular
                                           (qz-table-name name))))
          (qz-line 0 0 (format ", @$row->%s, " (qz-form-field name)))
          (qz-line 0 neol (format "'class=\"%s\"');?>" input-class)))
       (concat
        (qz-line (+ ntab 2) 0 (format "<%s " input-tag))
        (qz-line 0 0 (format "class=\"%s\" " input-class))
        (qz-line 0 0 (format "%s " input-type))
        (qz-line 0 0 (format "name=\"%s\" " (qz-form-field name)))
        (qz-line 0 0 (format "%s<?php echo " value-open))
        (qz-line 0 0 (format "@$row->%s;?>" (qz-form-field name)))
        (qz-line 0 neol (format "%s" value-close))))
     (qz-line (+ ntab 2) 0 "<?php echo form_error")
     (qz-line 0 neol (format "('%s'); ?>" (qz-form-field name)))
     (qz-line (+ ntab 1) neol "</div>")
     (qz-line ntab neol "</div>"))))

(defun qz-ci-form-horizontal (fields &optional ntab neol)
  "Create form horizontal from fields."
  (unless ntab
    (setq ntab 0))
  (unless neol
    (setq neol 1))
  (let ((name (qz-to-singular (qz-table-name fields)))
        (fst (car fields))
        (items (cdr (butlast (qz-localize-fields fields t))))
        (multipart (if (qz-ci-upload-exists fields)
                       (concat "accept-charset=\"utf-8\" "
                               "enctype=\"multipart/form-data\"") "")))
    (concat
     (qz-line ntab 0 "<form class=\"form-horizontal\" ")
     (qz-line 0 0 (format "name=\"form_%s\"" name))
     (qz-line 0 0 " method=\"post\" action=\"\" ")
     (qz-line 0 (+ neol 1) (format "%s>" multipart))
     (qz-line (+ ntab 1) 0 "<?php if (isset")
     (qz-line 0 neol (format "($%s)) { ?>" name))
     (qz-line (+ ntab 2) 0 "<?php $row = ")
     (qz-line 0 neol (format "$%s->row(); ?>" name))
     (qz-line (+ ntab 2) 0 "<input type=\"hidden\" name=")
     (qz-line 0 0 (format "\"%s\" " (qz-form-field fst)))
     (qz-line 0 0 "value=\"<?php echo $row->")
     (qz-line 0 neol (format "%s; ?>\" />" (qz-trim-field fst)))
     (qz-line (+ ntab 1) (+ neol 1) "<?php } ?>")
     (mapconcat
      #'(lambda (f)
          (qz-ci-form-horizontal-item f (+ ntab 1) neol))
      items
      (qz-line 0 1 ""))
     (qz-line 0 1 "")
     (qz-line (+ ntab 1) neol "<div class=\"form-group\">")
     (qz-line (+ ntab 2) 0 "<label class=\"control-label col-sm-2\">")
     (qz-line 0 neol "&nbsp;</label>")
     (qz-line (+ ntab 2) neol "<div  class=\"col-sm-10\">")
     (qz-line (+ ntab 3) 0 "<input class=\"btn btn-primary\" ")
     (qz-line 0 0 "type=\"submit\" name=\"btn_submit\" ")
     (qz-line 0 neol "value=\"<?php echo $btn_value;?>\" />")
     (qz-line (+ ntab 2) neol "</div>")
     (qz-line (+ ntab 1) neol "</div>")
     (qz-line 0 1 "")
     (qz-line ntab neol "</form>"))))



(defun qz-ci-table-header (fields &optional ntab neol exception)
  "Create table header from fields."
  (unless ntab
    (setq ntab 0))
  (unless neol
    (setq neol 1))
  (unless exception
    (setq exception nil))
  (let ((headers (butlast
                  (qz-exclude-keys
                   (qz-localize-fields
                    fields
                    (not exception))))))
    (concat
     (qz-line ntab neol "<th>No</th>")
     (mapconcat
      #'(lambda (h)
          (qz-line
           ntab
           neol
           (format
            "<th>%s</th>"
            (replace-regexp-in-string
             "_"
             " "
             (qz-upper-first
              (qz-form-field h ))))))
      headers "")
     (qz-line ntab neol "<th colspan=\"3\">Aksi</th>"))))

(defun qz-ci-table-content (fields &optional ntab neol exception)
  "Create table content from fields."
  (unless ntab
    (setq ntab 0))
  (unless neol
    (setq neol 1))
  (unless exception
    (setq exception nil))
  (let ((table (qz-table-name fields))
        (contents (butlast
                   (qz-exclude-keys
                    (qz-localize-fields
                     fields
                     (not exception))))))
    (concat
     (qz-line ntab neol "<td><?php echo $no++; ?></td>")
     (mapconcat
      #'(lambda (c)
          (qz-line
           ntab
           neol
           (format
            "<td><?php echo $row->%s;?></td>"
            (if (string-equal table (qz-table-name c))
                (qz-trim-field c)
              (qz-form-field c)))))
      contents ""))))

(defun qz-ci-flash-message (&optional ntab neol name)
  "Create condition to pop flashdata message."
  (unless ntab
    (setq ntab 0))
  (unless neol
    (setq neol 1))
  (unless name
    (setq name "pesan"))
  (concat
   (qz-line ntab 0 "<?php if ($this->session->flashdata")
   (qz-line 0 neol (format "('%s')) { ?>" name))
   (qz-line (+ ntab 1) 0 "<div class=\"alert alert-success\" ")
   (qz-line 0 0 "role=\"alert\">")
   (qz-line 0 0 "<?php echo $this->session->flashdata")
   (qz-line 0 neol (format "('%s');?></div>" name))
   (qz-line ntab neol "<?php } ?>")))

(provide 'qzuma-ci-core)
;;; qzuma-ci-core.el ends here
