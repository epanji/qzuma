;;; qzuma-tcgciws.el --- A table to code generator for service in codeigniter.  -*- lexical-binding: t; -*-

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

;; Generate function from controller in codeigniter using table fields.

;;; Code:

(require 'qzuma-core)

(defun qz-tcgciws-controller-for-web-service (name)
  "Create controller for web service"
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

(defun qz-tcgciws-function-from-fields (fields name &optional controller)
  "Create function from table fields."
  (if (qz-table-p fields)
      (progn
        (unless controller
          (setq controller "Android"))
        (qz-open-continue-buffer (concat controller ".php"))
        (when (equal 1 (point-at-bol))
          (insert (qz-tcgciws-controller-for-web-service controller))
          (forward-line -1))
        (when (fboundp 'web-mode) (web-mode))
        (insert
         (qz-line 0 1 "")
         (qz-line 1 1 (format "public function %s() {" name))
         (qz-line 2 1 "$data = array();")
         (qz-line 2 2 "$data['result'] = 'false';")

         ;; content get / set or if-get
         (qz-tcgciws-function-contents fields 2)

         (qz-line 0 1 "")
         (qz-line 2 1 "header('Content-Type: application/json');")
         (qz-line 2 1 "echo json_encode($data);")
         (qz-line 1 0 "}")))
    (print "Selected region not well formatted")))

(defun qz-tcgciws-function-contents (fields &optional ntab neol)
  "Choose content type first before execution."
  (unless ntab
    (setq ntab 0))
  (unless neol
    (setq neol 0))
  (let* ((choise '(("get" . "get")
                   ("set" . "set")
                   ("if-get" . "if-get")))
         (choosen (completing-read
                   "Function Mode (get): " ; prompt
                   choise                  ; collection
                   nil                     ; predicate
                   t                       ; require-match
                   nil                     ; initial-input
                   nil                     ; hist
                   "get"                   ; def
                   )))
    (cond ((equal choosen "get")
           (qz-tcgciws-function-content-get fields ntab neol))
          ((equal choosen "set")
           (qz-tcgciws-function-content-set fields ntab neol))
          ((equal choosen "if-get")
           (qz-tcgciws-function-content-if-get fields ntab neol)))))

(defun qz-tcgciws-function-content-get (fields &optional ntab neol)
  "Content for request data."
  (unless ntab
    (setq ntab 0))
  (unless neol
    (setq neol 0))
  (concat
   (qz-tcgciws-get-content
    (butlast (qz-localize-fields fields)) ntab neol)
   (qz-line 0 1 "")
   (qz-tcgciws-get-query-content fields ntab neol)))

(defun qz-tcgciws-get-content (fields &optional ntab neol)
  "Select option to get."
  (unless ntab
    (setq ntab 0))
  (unless neol
    (setq neol 0))
  (let ((one (car fields))
        (body (cdr fields))
        (container ""))
    (setq
     container
     (qz-line ntab 1 (format "$select  = \"%s, \";" one)))
    (when (> (length body) 1)
      (mapc
       (lambda (field)
         (setq
          container
          (concat
           container
           (qz-line ntab 1 (format "$select .= \"%s, \";" field)))))
       (butlast body)))
    (concat
     container
     (qz-line ntab 1 (format "$select .= \"%s \";" (car (reverse body)))))))

(defun qz-tcgciws-get-join-fields (fields)
  "Filter fields for join."
  (let ((names (rest (butlast fields))))
    (delq
     nil
     (mapcar
      (lambda (field)
        (when (qz-identity-p field)
          field))
      names))))

(defun qz-tcgciws-get-filtered-fields (fields &optional string)
  "Get fields after filtered."
  (unless string
    (setq string "fields"))
  (completing-read-multiple
   (concat
    "Optional " string " with comma (nil): ") ; prompt
   (butlast fields)                           ; collection
   nil                                        ; predicate
   t                                          ; require-match
   nil                                        ; initial-input
   nil                                        ; hist
   nil                                        ; def
   ))

(defun qz-tcgciws-get-query-content (fields &optional ntab neol wheres)
  "Query to get data"
  (unless ntab
    (setq ntab 0))
  (unless neol
    (setq neol 0))
  (let ((table (qz-table-name fields))
        (flag (car (reverse fields)))
        (joins (qz-tcgciws-get-join-fields fields)))
    (concat
     (qz-line ntab 1 "$this->db->select($select);")
     (when joins
       (let ((container ""))
         (mapc
          (lambda (id)
            (setq
             container
             (concat
              container
              (qz-line ntab 0 "$this->db->join(")
              (qz-line 0 0 (format "'%s', " (qz-table-name id)))
              (qz-line 0 0 (format "'%s = " id))
              (qz-line 0 1 (format "%s');" (qz-join-field table id))))))
          joins)
         container))
     (qz-line ntab 1 (format "$this->db->where('%s', '1');" flag))
     (when wheres
       (let ((container ""))
         (mapc
          (lambda (field)
            (setq
             container
             (concat
              container
              (qz-line ntab 0 "$this->db->where")
              (qz-line 0 0 (format "('%s', " field))
              (qz-line 0 0 "$this->input->post(")
              (qz-line 0 1 (format "'%s'));" (qz-form-field field))))))
          wheres)
         container))
     (qz-line ntab 0 (format "$q_%s = $this->" table))
     (qz-line 0 1 (format "db->get('%s');" table))
     (qz-line ntab 1 (format "if ($q_%s->num_rows() > 0) {" table))
     (qz-line (+ ntab 1) 0 "$data['data'] = ")
     (qz-line 0 1 (format "$q_%s->result();" table))
     (qz-line (+ ntab 1) 1 "$data['result'] = 'true';")
     (qz-line ntab 1 "}"))))

(defun qz-tcgciws-function-content-set (fields &optional ntab neol)
  "Content for input data."
  (unless ntab
    (setq ntab 0))
  (unless neol
    (setq neol 0))
  (concat
   (qz-tcgciws-fields-validation
    (cdr (butlast (qz-localize-fields fields t))) ntab neol)
   (qz-line 0 1 "")
   (qz-tcgciws-if-post-open
    (cdr (butlast (qz-localize-fields fields))) ntab neol)
   (qz-tcgciws-if-post-content
    (butlast (qz-localize-fields fields t)) (+ ntab 2) neol)
   (qz-line 0 1 "")
   (qz-tcgciws-if-post-insert-update
    (qz-localize-fields fields) (+ ntab 2) neol)
   (qz-line (+ ntab 2) 1 "$data['data'] = \"Sukses\";")
   (qz-tcgciws-if-post-close ntab neol t)))

(defun qz-tcgciws-function-content-if-get (fields &optional ntab neol)
  "Content with conditional term to access."
  (unless ntab
    (setq ntab 0))
  (unless neol
    (setq neol 0))
  (let ((valids (qz-tcgciws-get-filtered-fields
                 (qz-localize-fields fields) "validations")))
    (concat
     (qz-tcgciws-fields-validation valids ntab neol)
     (qz-line 0 1 "")
     (qz-tcgciws-if-post-open
      (butlast (qz-localize-fields fields)) ntab neol)
     (qz-tcgciws-get-content
      (butlast (qz-localize-fields fields)) (+ ntab 2) neol)
     (qz-line 0 1 "")
     (qz-tcgciws-get-query-content
      fields (+ ntab 2) neol
      (qz-tcgciws-get-filtered-fields
       (qz-localize-fields fields) "query conditions"))
     (qz-tcgciws-if-post-close ntab neol))))

(defun qz-tcgciws-if-post-insert-update (fields &optional ntab neol)
  "Input or update database."
  (unless ntab
    (setq ntab 0))
  (unless neol
    (setq neol 0))
  (let ((table (qz-table-name fields))
        (field (car fields)))
    (concat
     (qz-line ntab 0 (format "if ($%s = " (qz-form-field field)))
     (qz-line 0 0 "$this->input->post")
     (qz-line 0 1 (format "('%s')) {" (qz-form-field field)))
     (qz-line (+ ntab 1) 0 "$this->db->where")
     (qz-line 0 0 (format "('%s'" (qz-trim-field field)))
     (qz-line 0 1 (format ", $%s);" (qz-form-field field)))
     (qz-line (+ ntab 1) 0 "$this->db->update")
     (qz-line 0 1 (format "('%s', $data_%s);" table table))
     (qz-line ntab 1 "} else {")
     (qz-line (+ ntab 1) 0 "$this->db->insert")
     (qz-line 0 1 (format "('%s', $data_%s);" table table))
     (qz-line ntab 1 "}"))))

(defun qz-tcgciws-if-post-open (fields &optional ntab neol)
  "Content condition for input or conditional access."
  (unless ntab
    (setq ntab 0))
  (unless neol
    (setq neol 0))
  (let* ((choice fields)
         (choosen (completing-read
                   "Condition field: "   ; prompt
                   choice                ; collection
                   nil                   ; predicate
                   t                     ; require-match
                   nil                   ; initial-input
                   nil                   ; hist
                   (car choice)          ; def
                   )))
    (concat
     (qz-line ntab 0 "if ($this->input->post")
     (qz-line 0 1 (format "('%s')) {" (qz-form-field choosen)))
     (qz-line (+ ntab 1) 0 "if ($this->form_validation")
     (qz-line 0 1 "->run() !== FALSE) {")
     (qz-line 0 neol ""))))

(defun qz-tcgciws-if-post-content (fields &optional ntab neol)
  "Get data from input post."
  (unless ntab
    (setq ntab 0))
  (unless neol
    (setq neol 0))
  (let ((posts "")
        (table (qz-table-name fields)))
    (mapc
     (lambda (field)
       (setq
        posts
        (concat
         posts
         (qz-line ntab 0 (format "$data_%s" table))
         (qz-line 0 0 (format "['%s'] = " (qz-trim-field field)))
         (qz-line 0 0 "$this->input->post(")
         (qz-line 0 1 (format "'%s');" (qz-form-field field))))))
     (cdr fields))
    (qz-line 0 neol posts)))

(defun qz-tcgciws-if-post-close (&optional ntab neol result)
  "Close condition content."
  (unless ntab
    (setq ntab 0))
  (unless neol
    (setq neol 0))
  (concat
   (when result
     (qz-line (+ ntab 2) 1 "$data['result'] = 'true';"))
   (qz-line (+ ntab 1) 1 "} else {")
   (qz-line (+ ntab 2) 1 "$data['data'] = \"Gagal validasi\";")
   (qz-line (+ ntab 1) 1 "}")
   (qz-line ntab 1 "}")
   (qz-line 0 neol "")))

(defun qz-tcgciws-fields-validation (fields &optional ntab neol)
  "Get string validation."
  (unless ntab
    (setq ntab 0))
  (unless neol
    (setq neol 0))
  (let ((validations ""))
    (unless fields
      (setq
       validations
       (qz-line ntab 1 "// need validation.")))
    (mapc
     (lambda (field)
       (setq
        validations
        (concat
         validations
         (qz-line ntab 0 "$this->form_validation->set_rules(")
         (qz-line 0 0 (format "'%s', " (qz-form-field field)))
         (qz-line 0 0 (format "'%s', " (qz-trim-field field)))
         (qz-line 0 1 "'required');"))))
     fields)
    (qz-line 0 neol validations)))

;;; commands

(defun qz-tcgciws-create-codeigniter-web-service ()
  "Start creating file buffer for services."
  (interactive)
  (let ((controller
         (read-from-minibuffer
          "Controller name: "
          "Android")))
    (qz-open-clear-buffer (concat controller ".php"))
    (insert (qz-tcgciws-controller-for-web-service controller))
    (forward-line -1)
    (when (fboundp 'web-mode) (web-mode))))

(defun qz-tcgciws-create-codeigniter-web-service-function ()
  "Create function from region."
  (interactive)
  (if (use-region-p)
	  (let* ((fields (qz-list-from-region
                      (region-beginning)
                      (region-end)))
             (controller (read-from-minibuffer
                          "Controller name: "
                          "Android"))
             (name (read-from-minibuffer
                    "Function name: "
                    (qz-table-name fields))))
        (qz-tcgciws-function-from-fields fields name controller))
	(print "No region selected")))

(provide 'qzuma-tcgciws)
;;; qzuma-tcgciws.el ends here
