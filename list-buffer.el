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

(defvar *buffer-list* nil
  "List associated with the current list buffer.")

(defvar *buffer-headers* nil
  "Headers associated with the current list buffer.")

(defun list-buffer-create (buffer list &optional headers)
  (pop-to-buffer buffer)
  (set (make-local-variable '*buffer-list*) list)
  (set (make-local-variable '*buffer-headers*) headers)
  ;; set commands at the bottom
  (list-buffer-refresh))

(defun list-format-row (widths row)
  (apply #'concat
         (cl-mapcar
          (lambda (width cell)
            (if (< (length cell) width)
                (concat cell (make-list (- width (length cell)) ?\ ))
              (concat (subseq cell 0 (- width 4)) "... ")))
          widths row)))

(defun list-buffer-refresh ()
  (let* ((strings (mapcar (lambda (row)
                            (mapcar (lambda (cell) (format "%s" cell)) row))
                          *buffer-list*))
         (lengths (mapcar (lambda (row) (mapcar #'length row)) strings))
         (widths (apply #'cl-mapcar #'max lengths)))
    ;; write headers
    (when *buffer-headers*
      (set (make-local-variable 'header-line-format)
           (list-format-row widths *buffer-headers*)))
    ;; write rows
    (delete (point-min) (point-max))
    (insert (mapconcat (lambda (row) (list-format-row widths row))
                       strings "\n"))))

(defun list-buffer-sort (key predicate)
  (set *buffer-list* (cl-sort *buffer-list* predicate :key key)))

(defun list-buffer-filter (key filter)
  (set *buffer-list* (cl-remove-if-not *buffer-list* filter :key key)))

(provide 'list-buffer)
