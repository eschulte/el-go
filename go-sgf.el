;;; go-sgf.el --- SGF back end

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

;; Commentary:

;; This file implements an `go-trans' interface into an SGF file.

;; Code:
(require 'go-util)
(require 'go-trans)

(defun go-sgf-nthcdr (sgf index)
  (let ((part sgf))
    (while (cdr index)
      (setq part (nth (car index) part))
      (setq index (cdr index)))
    (setq part (nthcdr (car index) part))
    part))

(defun go-sgf-ref (sgf index)
  (let ((part sgf))
    (while (car index)
      (setq part (nth (car index) part))
      (setq index (cdr index)))
    part))

(defun set-go-sgf-ref (sgf index new)
  (eval `(setf ,(reduce (lambda (acc el) (list 'nth el acc))
                        index :initial-value 'sgf)
               ',new)))

(defsetf go-sgf-ref set-go-sgf-ref)


;;; Class and interface
(defclass sgf nil
  ((self  :initarg :self  :accessor self  :initform nil)
   (index :initarg :index :accessor index :initform '(0)))
  "Class for the SGF back end.")

(defmethod current ((sgf sgf))
  (sgf-ref (self sgf) (index sgf)))

(defmethod root ((sgf sgf))
  (sgf-ref (self sgf) '(0)))

(defmethod go->move ((sgf sgf) move))

(defmethod go->board ((sgf sgf) size))

(defmethod go->resign ((sgf sgf) resign))

(defmethod go->undo ((sgf sgf))
  (decf (car (last (index sgf))))
  (alistp (current sgf)))

;; (defmethod go->comment ((sgf sgf) comment)
;;   ;; TODO: need a setf method for current
;;   (push (cons :C comment) (current sgf)))

(defmethod go<-size ((sgf sgf))
  (or (aget (root sgf) :S)
      (aget (root sgf) :SZ)))

(defmethod go<-name ((sgf sgf))
  (or (aget (root sgf) :GN)
      (aget (root sgf) :EV)))

(defmethod go<-alt ((sgf sgf)))

(defmethod go<-turn ((sgf sgf) color)
  (incf (car (last (index sgf))))
  (current sgf))

(defmethod go<-comment ((sgf sgf))
  (aget (current sgf) :C))

(defun go-from-file (file)
  (interactive "f")
  (make-instance 'sgf :self (sgf2el-file-to-el file)))

(provide 'go-sgf)
