;;; qzuma-macro.el --- An experiment macro in emacs.  -*- lexical-binding: t; -*-

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

;; This file contains the core macro in all qzuma package.

;;; Code:

(defmacro qz-line (ntab neol &rest body)
  "Create new line with numbered tab and numbered end-of-line.
The argument to this command are as follow:

NTAB: an integer of character tab.
NEOL: an integer of character end-of-line.
BODY: sequences of argument which each argument may be a string or a
      list or vector of characters (integers) list of string or
      expression that return string body of line."
  `(let ((n1 (if (integerp ,ntab) ,ntab 0))
         (n2 (if (integerp ,neol) ,neol 0)))
     (concat (make-string n1 ?\t) ,@body (make-string n2 ?\n))))

(defmacro qz-define-read-prompt (prefix read-name key-string alist-string
                                        &optional html-template)
  "Create variable default, function non-interactive and interactive
from association list string with optional template as wrapper.
When call-interactively, minibuffer will prompt available options from
association list.

The argument to this command are as follow:

PREFIX:         a string to make it uniq.
READ-NAME:      a string as a name of single collection alist.
KEY-STRING:     a string as default value taken from alist key.
ALIST-STRING:   an association list with key and value as string.
HTML-TEMPLATE:  an optional string as template with single '%s' inside."
  (unless html-template
    (setq html-template "%s"))
  (when (and (stringp prefix)
             (stringp read-name)
             (stringp key-string)
             (stringp (cdar (symbol-value alist-string)))
             (stringp html-template))
    (let((default (concat prefix "-default-" read-name))
         (read (concat prefix "-read-" read-name))
         (get (concat prefix "-get-" read-name))
         (insert (concat prefix "-insert-" read-name)))
      `(progn
         (defvar ,(intern default) nil)
         (setq ,(intern default) ,key-string)
         (defun ,(intern read) ()
           ,(concat "Read from minibuffer with completion.\n\n"
                    "This function generated by "
                    "`qz-define-read-prompt'.")
           (completing-read
            (format
             ,(concat (capitalize read-name) " Name (%s): ")
             ,(intern default)) ; prompt
            ,alist-string       ; collection
            nil                 ; predicate
            t                   ; require-match
            nil                 ; initial-input
            nil                 ; hist
            ,(intern default)   ; def
            ))
         (defun ,(intern get) (name &optional html)
           ,(concat "Non-interactive get association value from \`"
                    (symbol-name alist-string) "\'.\nIf non-nil HTML, "
                    "wrapped this value as defined.\n\n"
                    "This function generated by "
                    "`qz-define-read-prompt'.")
           (let ((template "%s")
                 (,(intern read-name) (cdr (assoc name ,alist-string))))
             (unless ,(intern read-name)
               (setq ,(intern read-name) ""))
             (when html
               (setq template ,html-template))
             (format template ,(intern read-name))))
         (defun ,(intern insert) (name &optional html)
           ,(concat "Insert association value from \`"
                    (symbol-name alist-string)
                    "\' at the point.\nIf non-nil HTML, "
                    "wrapped this value as defined.\n\n"
                    "This function generated by "
                    "`qz-define-read-prompt'.")
           (interactive
            (list (,(intern read))
                  current-prefix-arg))
           (setq ,(intern default) name)
           (if html
               (insert (,(intern get) name 1))
             (insert (,(intern get) name))))))))

(defmacro qz-define-list-range (list-name max &rest body)
  "Create variable association list from BODY with addition dash and
number at the end of each string. Number begin from 1 to MAX as user
defined.

The argument to this command are as follow:

LIST-NAME:  a string as variable name for alist.
MAX:        a number maximum to be added at the end of string \"-%d\".
BODY:       a cons as association list."
  (let ((count 0)
        (container ()))
    (while (< count max)
      (mapc
       #'(lambda (item)
           (setq container
                 (append container
                         (list
                          (cons
                           (format "%s-%d" (car item) (+ count 1))
                           (format "%s-%d" (cdr item) (+ count 1)))))))
       body)
      (setq count (+ count 1)))
    `(defvar ,(intern list-name) nil)
    `(setq ,(intern list-name) '(,@container))))

(defmacro qz-define-field-type (prefix type-name alias-type &rest body)
  "Create variable string sequence with predicate function represent
with variable name.

The argument to this command are as follow:

PREFIX:      a string to make it uniq.
TYPE-NAME:   a string as a name of single collection list string (RegEx).
ALIAS-TYPE:  a string true name for the type.
BODY:        sequences of argument which each argument must be a string
             represent regular expression."
  (let ((name (concat prefix "-" type-name))
        (alias (concat prefix "-" type-name "-alias"))
        (body (seq-filter #'stringp body)))
    `(prog1
         (defvar ,(intern name) nil)
       (setq ,(intern name) ',body)
       (defvar ,(intern alias) nil)
       (setq ,(intern alias) ,alias-type)
       (defun ,(intern (concat name "-p")) (field)
         ,(concat "Check if field matches with variable type.\n\n"
                  "This function generated by "
                  "`qz-define-field-type'.")
         (let ((match (mapcar #'(lambda (type) (string-match type field))
                              ,(intern name))))
           (if (remq nil match) t nil))))))

(provide 'qzuma-macro)
;;; qzuma-macro.el ends here
