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

(require 'qzuma-ci-core)

;; functions

(defun qz-tcgci-method (fields name &optional class type parameters)
  "Create function from table fields inside class."
  (if (qz-table-p fields)
      (progn
        (unless class
          (setq class "Debug"))
        (unless type
          (setq type "global"))
        (unless parameters
          (setq parameters ""))
        (qz-open-continue-buffer (concat class ".php"))
        (when (equal 1 (point-at-bol))
          (insert (qz-ci-class-wrapper name type (qz-table-name fields)))
          (forward-line -1))
        (when (fboundp 'web-mode) (web-mode))
        (insert
         (qz-line 0 1 "")
         (qz-line 1 0 (format "public function %s" name))
         (qz-line 0 1 (format "(%s) {" parameters))
         (qz-tcgci-method-contents fields name 2 1 class)
         (qz-ci-method-footer type class name)
         (qz-line 1 0 "}")))
    (print "Selected region not well formatted")))

(defun qz-tcgci-method-contents (fields name &optional ntab neol controller)
  "Create contents from fields."
  (unless ntab
    (setq ntab 0))
  (unless neol
    (setq neol 1))
  (let ((table (qz-table-name fields)))
    (cond
     ((string-equal name "create")
      (concat
       (qz-line ntab 0 (format "$data_%s" table))
       (qz-line 0 2" = array();")
       (qz-ci-data-post fields ntab neol)
       (qz-line 0 1 "")
       (qz-line ntab 0 "$this->db->insert(")
       (qz-line 0 0 (format "'%s', " table))
       (qz-line 0 neol (format "$data_%s);" table))
       (qz-line ntab neol "return $this->db->insert_id();")))
     ((string-equal name "read")
      (concat
       (qz-line ntab neol "if ($is_id) {")
       (qz-line (+ ntab 1) 0 "$this->db->where")
       (qz-line 0 neol (format "('%s.flag', 1);" table))
       (qz-line (+ ntab 1) 0 "$this->db->where")
       (qz-line 0 neol (format "('%s.id', $str_or_id);" table))
       (qz-line ntab neol "} else {")
       (qz-line (+ ntab 1) neol "$config['base_url'] = $base_url;")
       (qz-line (+ ntab 1) neol "$config['per_page'] = $per_page;")
       (qz-line (+ ntab 1) 0 "$config['reuse_query_string'] ")
       (qz-line 0 (+ neol 1) "= true;")
       (qz-ci-query-select-multi-line fields (+ ntab 1) neol)
       (qz-line 0 1 "")
       (qz-line (+ ntab 1) neol "$this->db->flush_cache();")
       (qz-line (+ ntab 1) neol "$this->db->start_cache();")
       (qz-line (+ ntab 1) neol "$this->db->select($select);")
       (qz-ci-data-join fields (+ ntab 1) neol)
       (qz-line (+ ntab 1) 0 "$this->db->where")
       (qz-line 0 neol (format "('%s.flag', '1');" table))
       (qz-line (+ ntab 1) neol "if ($str_or_id != '') {")
       (qz-line (+ ntab 2) 0 "$this->db->like")
       (qz-line 0 0 (format "('%s'" (car (qz-exclude-keys fields))))
       (qz-line 0 neol ", $str_or_id);")
       (qz-line (+ ntab 1) neol "}")
       (qz-line (+ ntab 1) (+ neol 1) "$this->db->stop_cache();")
       (qz-line (+ ntab 1) 0 "$config['total_rows'] = ")
       (qz-line 0 0 "$this->db->count_all_results")
       (qz-line 0 neol (format "('%s');" table))
       (qz-line (+ ntab 1) 0 "$this->pagination")
       (qz-line 0 (+ neol 1) "->initialize($config);")
       (qz-line (+ ntab 1) neol "if ($per_page != '') {")
       (qz-line (+ ntab 2) neol "$this->db->limit($per_page, $from);")
       (qz-line (+ ntab 1) neol "}")
       (qz-line ntab neol "}")
       (qz-line ntab 0 "return $this->db->get")
       (qz-line 0 neol (format "('%s');" table))))
     ((string-equal name "update")
      (concat
       (qz-line ntab 0 (format "$data_%s" table))
       (qz-line 0 neol" = array();")
       (qz-line ntab neol "if ($new_flag == '') {")
       (qz-ci-data-post fields (+ ntab 1) neol)
       (qz-line ntab neol "} else {")
       (qz-line (+ ntab 1) neol "if ($old_flag != '') {")
       (qz-line (+ ntab 2) 0 "$data_flag")
       (qz-line 0 0 (format "['%s.flag'] = " table))
       (qz-line 0 neol "$old_flag;")
       (qz-line (+ ntab 2) 0 "$this->db->where")
       (qz-line 0 0 (format "('%s.flag', " table))
       (qz-line 0 neol "$new_flag);")
       (qz-line (+ ntab 2) 0 "$this->db->update")
       (qz-line 0 neol (format "('%s', $data_flag);" table))
       (qz-line (+ ntab 1) neol "}")
       (qz-line (+ ntab 1) 0 (format "$data_%s['%s." table table))
       (qz-line 0 neol "flag'] = $new_flag;")
       (qz-line ntab neol "}")
       (qz-line ntab 0 "$this->db->where")
       (qz-line 0 neol (format "('%s.id', $id);" table))
       (qz-line ntab 0 "$this->db->update")
       (qz-line 0 neol (format "('%s', $data_%s);" table table))))
     ((string-equal name "delete")
      (concat
       (qz-line ntab 0 "/* Preferable using ")
       (qz-line 0 neol "update($id, 0) than this. */")
       (qz-line ntab 0 "#$this->db->where")
       (qz-line 0 neol (format "('%s.id', $id);" table))
       (qz-line ntab 0 "#return $this->db->delete")
       (qz-line 0 neol (format "('%s');" table))))
     ((string-equal name "index")
      (concat
       (qz-line ntab (+ neol 1) "$data = array();")
       (qz-line ntab neol "$data['no'] = $page + 1;")
       (qz-line ntab neol "$per_page = 10;")
       (qz-line ntab 0 "$base_url = base_url()")
       (qz-line 0 neol (format ".'%s';" (downcase controller)))
       (qz-line ntab 0 "if(! $search = $this->input->get")
       (qz-line 0 neol (format "('%s_search')) {" (downcase controller)))
       (qz-line (+ ntab 1) neol "$search = '';")
       (qz-line ntab neol "}")
       (qz-line ntab 0 (format "$data['%s'] = " table))
       (qz-line 0 0 (format "$this->%s_model" table))
       (qz-line 0 0 "->read($search, false, $per_page, ")
       (qz-line 0 neol "$base_url, $page);")))
     ((string-equal name "add")
      (concat
       (qz-line ntab (+ neol 1) "$data = array();")
       (qz-line ntab 0 "$this->form_validation->")
       (qz-line 0 0 "set_error_delimiters('<br />")
       (qz-line 0 neol "<span class=\"error\">', '</span>');")
       (qz-ci-data-validation fields ntab neol)
       (qz-line 0 1 "")
       (qz-line ntab neol "if ($this->input->post('btn_submit')) {")
       (qz-line (+ ntab 1) 0 "if ($this->form_validation->run() ")
       (qz-line 0 neol "!== FALSE) {")
       (qz-line (+ ntab 2) 0 (format "$this->%s_model" table))
       (qz-line 0 neol "->create();")
       (qz-line (+ ntab 2) 0 "$this->session->set_flashdata")
       (qz-line 0 neol "('pesan', 'Data berhasil disimpan');")
       (qz-line (+ ntab 2) 0 "redirect(base_url()")
       (qz-line 0 neol (format ".'%s/');" (downcase controller)))
       (qz-line (+ ntab 1) neol "}")
       (qz-line ntab neol "}")))
     ((string-equal name "edit")
      (concat
       (qz-line ntab (+ neol 1) "$data = array();")
       (qz-line ntab 0 (format "$data['%s'] = " table))
       (qz-line 0 0 (format "$this->%s" table))
       (qz-line 0 (+ neol 1) "_model->read($id, true);")
       (qz-line ntab 0 "$this->form_validation->")
       (qz-line 0 0 "set_error_delimiters('<br />")
       (qz-line 0 neol "<span class=\"error\">', '</span>');")
       (qz-ci-data-validation fields ntab neol)
       (qz-line 0 1 "")
       (qz-line ntab neol "if ($this->input->post('btn_submit')) {")
       (qz-line (+ ntab 1) 0 "if ($this->form_validation->run() ")
       (qz-line 0 neol "!== FALSE) {")
       (qz-line (+ ntab 2) 0 (format "$this->%s_model" table))
       (qz-line 0 neol "->update($id);")
       (qz-line (+ ntab 2) 0 "$this->session->set_flashdata")
       (qz-line 0 neol "('pesan', 'Data berhasil diperbarui');")
       (qz-line (+ ntab 2) 0 "redirect(base_url()")
       (qz-line 0 neol (format ".'%s/');" (downcase controller)))
       (qz-line (+ ntab 1) neol "}")
       (qz-line ntab neol "}")))
     ((string-equal name "remove")
      (concat
       (qz-line ntab 0 (format "$this->%s" table))
       (qz-line 0 neol "_model->update($id, '0');")
       (qz-line ntab 0 "$this->session->set_flashdata")
       (qz-line 0 neol "('pesan', 'Data berhasil dihapus');")
       (qz-line ntab 0 "redirect(base_url()")
       (qz-line 0 neol (format ".'%s/');" (downcase controller)))))
     ((string-equal name "detail")
      (concat "controller->detail for read data detail"))
     )))

(defun qz-tcgci-upload-exists (fields &optional ntab neol)
  "Add upload function for the type if exists."
  (unless ntab
    (setq ntab 0))
  (unless neol
    (setq neol 1))
  (let ((uploads (qz-ci-upload-exists fields)))
    (if uploads
        (concat
         (qz-line 0 1 "")
         (qz-line 0 1 (qz-ci-define-private-upload ntab neol))
         (mapconcat
          #'(lambda (type)
              (qz-ci-define-function-upload type ntab neol))
          uploads (qz-line 0 1 ""))) "")))

;; commands

(defun qz-tcgci-create-controller ()
  "Create controller from table fields in region."
  (interactive)
  (if (use-region-p)
	  (let ((fields (qz-list-from-region
                     (region-beginning)
                     (region-end)))
            (type "controller")
			(controller (qz-upper-first (read-from-minibuffer
                                         "Controller name: "))))
		(if (qz-table-p fields)
			(progn
              (qz-open-clear-buffer (format "%s.php" controller))
              (insert (qz-ci-class-wrapper controller type (qz-table-name fields)))
              (qz-open-continue-buffer (concat controller ".php"))
              (qz-tcgci-method fields "index" controller type "$page = 0")
              (qz-tcgci-method fields "add" controller type)
              (qz-tcgci-method fields "edit" controller type "$id")
              (qz-tcgci-method fields "remove" controller "" "$id")
              ;; ok
              )
		  (print "Selected region not well formatted")))
	(print "No region selected")))

(defun qz-tcgci-create-model ()
  "Create model from table fields in region."
  (interactive)
  (if (use-region-p)
	  (let* ((fields (qz-list-from-region
                      (region-beginning)
                      (region-end)))
             (type "model")
             (model (qz-upper-first
                     (qz-to-singular
                      (qz-table-name fields)))))
		(if (qz-table-p fields)
			(progn
              (qz-open-clear-buffer (format "%s_model.php" model))
              (insert (qz-ci-class-wrapper model type))
              (qz-open-continue-buffer (format "%s_model.php" model))
              (insert (qz-tcgci-upload-exists fields 1 1))
              (qz-tcgci-method fields "create" (format "%s_model" model))
              (qz-tcgci-method
               fields "read" (format "%s_model" model) type
               (concat "$str_or_id = '', "
                       "$is_id = false, "
                       "$per_page = '', "
                       "$base_url = '', "
                       "$from = 0"))
              (qz-tcgci-method
               fields "update" (format "%s_model" model) type
               "$id, $new_flag = '', $old_flag = ''")
              (qz-tcgci-method
               fields "delete" (format "%s_model" model) type "$id"))
		  (print "Selected region not well formatted")))
	(print "No region selected")))

(provide 'qzuma-tcgci)
;;; qzuma-tcgci.el ends here
