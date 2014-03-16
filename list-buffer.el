;;; list-buffer.el --- view a list as a table in a buffer

;; Copyright (C) 2013  Free Software Foundation, Inc.

;; Author: Eric Schulte <schulte.eric@gmail.com>
;; Created: 2013-08-02
;; Version: 0.1
;; Keywords: list buffer cl

;; This software is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(eval-when-compile (require 'cl))
(require 'go-util)

(defvar *buffer-list* nil
  "List associated with the current list buffer.")

(defvar *buffer-headers* nil
  "Headers associated with the current list buffer.")

(defvar *buffer-width* nil
  "Width associated with the current list buffer.")

(defvar *enter-function* nil
  "Function used to enter a list element.
The function should take two arguments, the current row and
column respectively and may access the current buffer list
through the `*buffer-list*' variable.")

(defvar *refresh-function* nil
  "Function used to refresh a list element or the whole list.
The function should take two arguments, the current row and
column respectively and may access the current buffer list
through the `*buffer-list*' variable.")

(defun list-buffer-create
  (buffer list &optional headers enter-function refresh-function)
  (pop-to-buffer buffer)
  (list-mode)
  (set (make-local-variable '*buffer-width*) (window-total-width))
  (set (make-local-variable '*buffer-list*) list)
  (set (make-local-variable '*buffer-headers*)
       (mapcar (curry #'format "%s") headers))
  (set (make-local-variable '*enter-function*)
       (or enter-function
           (lambda (row col)
             (message "enter %S" (nth col (nth row *buffer-list*))))))
  (set (make-local-variable '*refresh-function*)
       (or refresh-function
           (lambda (row col)
             (message "refresh %S" (nth col (nth row *buffer-list*))))))
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
  (funcall *enter-function* (list-current-row) (list-current-col)))

(defun list-refresh ()
  (interactive)
  (funcall *refresh-function* (list-current-row) (list-current-col)))

(defun list-filter ()
  (interactive)
  (error "not implemented."))

(defun list-move-col (direction)
  (cl-flet ((col () (or (get-text-property (point) :col) start-col)))
    (let ((start-col (col)))
      (while (= start-col (col))
        (case direction
          (:forward (forward-char))
          (:backward (backward-char))))
      (when (eql direction :backward)
        (let ((end-col (col)))
          (while (= end-col (col)) (backward-char))
          (forward-char))))))

(defun list-next-col () (interactive) (list-move-col :forward))
(defun list-prev-col () (interactive) (list-move-col :backward))

(defvar list-mode-map
  (let ((map (make-sparse-keymap)))
    ;; navigation
    (define-key map (kbd "j")               'next-line)
    (define-key map (kbd "k")               'previous-line)
    (define-key map (kbd "u")               'scroll-down-command)
    (define-key map (kbd "<tab>")           'list-next-col)
    (define-key map (kbd "<S-iso-lefttab>") 'list-prev-col)
    ;; list functions
    (define-key map (kbd "<up>")            'list-up)
    (define-key map (kbd "<down>")          'list-down)
    (define-key map (kbd "f")               'list-filter)
    (define-key map (kbd "r")               'list-refresh)
    (define-key map (kbd "RET")             'list-enter)
    (define-key map (kbd "q")               'bury-buffer)
    map)
  "Keymap for `list-mode'.")

(define-derived-mode list-mode nil "list"
  "Major mode for viewing a list.")

(provide 'list-buffer)
;;; list-buffer.el ends here
