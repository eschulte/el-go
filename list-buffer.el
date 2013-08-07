;;; list-buffer.el --- view a list as a table in a buffer

;; Copyright (C) 2013 Eric Schulte <schulte.eric@gmail.com>

;; Author: Eric Schulte <eric.schulte@gmx.com>
;; Created: 2013-08-02
;; Version: 0.1
;; Keywords: list buffer cl

;; This file is not (yet) part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(eval-when-compile (require 'cl))
(require 'go-util)

(defvar *buffer-list* nil
  "List associated with the current list buffer.")

(defvar *buffer-headers* nil
  "Headers associated with the current list buffer.")

(defvar *buffer-width* nil
  "Width associated with the current list buffer.")

(defun list-buffer-create (buffer list &optional headers)
  (pop-to-buffer buffer)
  (list-mode)
  (set (make-local-variable '*buffer-width*) (window-total-width))
  (set (make-local-variable '*buffer-list*) list)
  (set (make-local-variable '*buffer-headers*)
       (mapcar (curry #'format "%s") headers))
  ;; refresh every time the buffer changes size
  (set (make-local-variable 'window-size-change-functions)
       (cons (lambda (b)
               (when (or (not (numberp *buffer-width*))
                         (not (equal *buffer-width* (window-total-width))))
                 (set '*buffer-width* (window-total-width))
                 (list-buffer-refresh)))
             window-size-change-functions))
  (goto-char (point-min))
  (list-buffer-refresh))

(defun list-format-row (widths row &optional row-num)
  (cl-flet ((num (type number string)
                 (put-text-property 0 (length string) type number string)
                 string))
    (let ((col 0))
      (num :row row-num
           (apply #'concat
                  (cl-mapcar
                   (lambda (width cell)
                     (prog1
                         (num :col col
                              (if (< (length cell) width)
                                  (concat cell
                                          (make-list (- width (length cell))
                                                     ?\ ))
                                (concat (subseq cell 0 (- width 2)) "… ")))
                       (incf col)))
                   widths row))))))

(defun list-buffer-refresh ()
  (when *buffer-list*
    (let* ((start (point))
           (strings (mapcar (curry #'mapcar (curry #'format "%s")) *buffer-list*))
           (lengths (mapcar (curry #'mapcar #'length)
                            (if *buffer-headers*
                                (cons *buffer-headers* strings)
                              strings)))
           (widths (apply #'cl-mapcar (compose '1+ #'max) lengths))
           ;; scale widths by buffer width
           (widths (mapcar (compose #'floor (curry #'* (/ (window-total-width)
                                                (float (apply #'+ widths)))))
                           widths)))
      ;; write headers
      (when *buffer-headers*
        (set (make-local-variable 'header-line-format)
             (concat " " (list-format-row widths *buffer-headers*))))
      ;; write rows
      (delete-region (point-min) (point-max))
      (insert (mapconcat (compose (curry #'apply #'list-format-row widths) #'reverse)
                         (indexed strings) "\n"))
      (goto-char start))))

(defun list-buffer-sort (col predicate)
  (set '*buffer-list* (cl-sort *buffer-list* predicate :key (curry #'nth col)))
  (list-buffer-refresh))

(defun list-current-row () (get-text-property (point) :row))

(defun list-current-col () (get-text-property (point) :col))

(defun list< (a b)
  (cond
   ((and (numberp a) (numberp b) (< a b)))
   ((and (stringp a) (stringp b) (string< a b)))))

(defun list> (a b)
  (cond
   ((and (numberp a) (numberp b) (> a b)))
   ((and (stringp a) (stringp b) (string> a b)))))

(defun list-up ()
  (interactive)
  (list-buffer-sort (get-text-property (point) :col) #'list<))

(defun list-down ()
  (interactive)
  (list-buffer-sort (get-text-property (point) :col) #'list>))

(defun list-enter ()
  (interactive)
  (funcall *list-enter-function* (nth (list-current-row) *buffer-list*)))

(defun list-filter ()
  (interactive)
  (error "not implemented."))

(defvar list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<up>")    'list-up)
    (define-key map (kbd "<down>")  'list-down)
    (define-key map (kbd "f")       'list-filter)
    (define-key map (kbd "RET")     'list-enter)
    (define-key map (kbd "q")       'bury-buffer)
    map)
  "Keymap for `list-mode'.")

(define-derived-mode list-mode nil "list"
  "Major mode for viewing a list.")

(provide 'list-buffer)
