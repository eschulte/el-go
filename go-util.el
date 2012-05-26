;;; sgf-util.el --- utility functions for sgf-mode

;; Copyright (C) 2012 Eric Schulte <eric.schulte@gmx.com>

;; Author: Eric Schulte <eric.schulte@gmx.com>
;; Created: 2012-05-15
;; Version: 0.1
;; Keywords: game go sgf

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

(defun range (a &optional b)
  (block nil
    (let (tmp)
      (unless b
        (cond ((> a 0) (decf a))
              ((= a 0) (return nil))
              ((> 0 a) (incf a)))
        (setq b a a 0))
      (if (> a b) (setq tmp a a b b tmp))
      (let ((res (number-sequence a b)))
        (if tmp (nreverse res) res)))))

(defun take (num list) (subseq list 0 num))

(defmacro until (test &rest body)
  (declare (indent 1))
  `(while (not ,test) ,@body))

(defun alistp (list)
  (and (listp list)
       (listp (car list))
       (not (listp (caar list)))))

(defun pos-to-index (pos size)
  (+ (car pos) (* (cdr pos) size)))

(defun transpose-array (board)
  (let ((size (round (sqrt (length board))))
        (trans (make-vector (length board) nil)))
    (dotimes (row size trans)
      (dotimes (col size)
        (setf (aref trans (pos-to-index (cons row col) size))
              (aref board (pos-to-index (cons col row) size)))))))

(defun ear-muffs (str) (concat "*" str "*"))

(provide 'sgf-util)
