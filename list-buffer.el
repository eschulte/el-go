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

(defun list-buffer-create (buffer list &optional headers)
  (pop-to-buffer buffer)
  (set (make-local-variable '*buffer-list*) list)
  (set (make-local-variable '*buffer-headers*)
       (mapcar (curry #'format "%s") headers))
  ;; refresh every time the buffer changes size
  (set (make-local-variable 'window-size-change-functions)
       (cons (lambda (b) (list-buffer-refresh)) window-size-change-functions))
  ;; set commands at the bottom
  (list-buffer-refresh))

(defun list-format-row (widths row)
  (apply #'concat
         (cl-mapcar
          (lambda (width cell)
            (if (< (length cell) width)
                (concat cell (make-list (- width (length cell)) ?\ ))
              (concat (subseq cell 0 (- width 2)) "… ")))
          widths row)))

(defun list-buffer-refresh ()
  (let* ((strings (mapcar (curry #'mapcar (curry #'format "%s")) *buffer-list*))
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
    (insert (mapconcat (curry #'list-format-row widths) strings "\n")))
  (goto-char (point-min)))

(defun list-buffer-sort (col predicate)
  (set *buffer-list* (cl-sort *buffer-list* predicate :key (curry #'nth col)))
  (list-buffer-refresh))

(defun list-buffer-filter (col filter)
  (set *buffer-list* (cl-remove-if-not *buffer-list* filter :key (curry #'nth col)))
  (list-buffer-refresh))

(provide 'list-buffer)
