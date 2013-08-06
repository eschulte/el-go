;;; go-util.el --- utility functions for GO functions

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

(defun curry (function &rest arguments)
  (lexical-let ((function function)
                (arguments arguments))
    (lambda (&rest more) (apply function (append arguments more)))))

(defun rcurry (function &rest arguments)
  (lexical-let ((function function)
                (arguments arguments))
    (lambda (&rest more) (apply function (append more arguments)))))

(defun compose (function &rest more-functions)
  (cl-reduce (lambda (f g)
               (lexical-let ((f f) (g g))
                 (lambda (&rest arguments)
                   (funcall f (apply g arguments)))))
             more-functions
             :initial-value function))

(defun rcons (x lst)
  (append lst (list x)))

(defmacro rpush (x place)
  "Insert X at the back of the list stored in PLACE."
  (if (symbolp place) (list 'setq place (list 'rcons x place))
    (list 'callf2 'rcons x place)))

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

(defun set-aget (list key new)
  (if (aget list key)
      (setf (cdr (assoc key list)) new)
    (setf (cdr (last list)) (list (cons key new)))))

(defsetf aget set-aget)

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

(defun un-ear-muffs (str)
  (let ((pen-ult (1- (length str))))
    (if (and (= ?\* (aref str 0))
             (= ?\* (aref str pen-ult)))
        (substring str 1 pen-ult)
      str)))

(defun char-to-num (char)
  (cl-flet ((err () (error "gtp: invalid char %s" char)))
    (cond
     ((< char ?A)  (err))
     ((< char ?I)  (- char ?A))
     ((<= char ?T) (1- (- char ?A)))
     ((< char ?a)  (err))
     ((< char ?i)  (- char ?a))
     ((<= char ?t) (1- (- char ?a)))
     (t (err)))))

(defun num-to-char (num)
  (cl-flet ((err () (error "gtp: invalid num %s" num)))
    (cond
     ((< num 1) (err))
     ((< num 9) (+ ?A (1- num)))
     (t         (+ ?A num)))))

(defun sym-cat (&rest syms)
  (intern (mapconcat #'symbol-name (delq nil syms) "-")))

(defun go-number-p (string)
  "If STRING represents a number return its value."
  (if (and (string-match "[0-9]+" string)
	   (string-match "^-?[0-9]*\\.?[0-9]*$" string)
           (= (length (substring string (match-beginning 0)
				 (match-end 0)))
	      (length string)))
      (string-to-number string)))

(defun go-clean-text-properties (string)
  (set-text-properties 0 (length string) nil string) string)

(provide 'go-util)
