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


;;; Class
(defclass sgf nil
  ((self  :initarg :self  :accessor self  :initform nil)
   (index :initarg :index :accessor index :initform '(0)))
  "Class for the SGF back end.")

(defun sgf-from-file (file)
  (interactive "f")
  (make-instance 'sgf :self (sgf2el-file-to-el file)))

(defmethod current ((sgf sgf))
  (go-sgf-ref (self sgf) (index sgf)))

(defun set-current (sgf new)
  (setf (go-sgf-ref (self sgf) (index sgf)) new))

(defsetf current set-current)

(defmethod root ((sgf sgf))
  (go-sgf-ref (self sgf) '(0)))

(defun set-root (sgf new)
  (if (self sgf)
      (setf (car (self sgf)) new)
    (setf (self sgf) (list new))))

(defsetf root set-root)

(defmethod next ((sgf sgf))
  (incf (car (last (index sgf)))))

(defmethod prev ((sgf sgf))
  (decf (car (last (index sgf)))))


;;; interface
(defmethod go-size ((sgf sgf))
  (or (aget (root sgf) :S)
      (aget (root sgf) :SZ)))

(defmethod set-go-size ((sgf sgf) size)
  (cond
   ((aget (root sgf)  :S) (setf (cdr (assoc  :S (root sgf))) size))
   ((aget (root sgf) :SZ) (setf (cdr (assoc :SZ (root sgf))) size))
   (t                     (push (cons :S size) (root sgf)))))

(defmethod go-name ((sgf sgf))
  (or (aget (root sgf) :GN)
      (aget (root sgf) :EV)))

(defmethod set-go-name ((sgf sgf) name)
  (cond
   ((aget (root sgf) :GN) (setf (cdr (assoc :GN (root sgf))) name))
   ((aget (root sgf) :EV) (setf (cdr (assoc :EV (root sgf))) name))
   (t                     (push (cons :GN name) (root sgf)))))

(defmethod go-move ((sgf sgf))
  (next sgf)
  (let ((turn (current sgf)))
    (if turn
        (or (assoc :B turn) (assoc :W turn))
      (prev sgf)
      (error "sgf: no more moves"))))

(defmethod set-go-move ((sgf sgf) move)
  (if (current sgf)
      ;; TODO: this overwrites rather than saving alternatives
      (setf (current sgf) (list move))
    (rpush (list move) (go-sgf-ref (self sgf) (butlast (index sgf))))))

(defmethod go-labels ((sgf sgf))
  (next sgf)
  (let ((turn (current sgf)))
    (if turn
        (remove-if (lambda (pair) (member (car pair) '(:B :W))) turn)
      (prev sgf)
      (error "sgf: no more moves"))))

(defmethod set-go-lables ((sgf sgf) labels)
  (if (current sgf)
      (setf (current sgf) (cons (or (assoc :B (current sgf))
                                    (assoc :W (current sgf)))
                                labels))
    (rpush labels (go-sgf-ref (self sgf) (butlast (index sgf))))))

(defmethod go-comment ((sgf sgf))
  (aget (current sgf) :C))

(defmethod set-go-comment ((sgf sgf) comment)
  (if (aget (current sgf) :C)
      (setf (cdr (assoc :C (current sgf))) comment)
    (push (cons :C comment) (current sgf))))

(defmethod go-alt ((sgf sgf))
  (error "sgf: go-alt not yet supported"))

(defmethod set-go-alt ((sgf sgf) alt)
  (error "sgf: set-go-alt not yet supported"))

(defmethod go-color ((sgf sgf))
  (signal 'unsupported-back-end-command (list sgf :move)))

(defmethod set-go-color ((sgf sgf) color)
  (signal 'unsupported-back-end-command (list sgf :set-color color)))

;; non setf'able generic functions
(defmethod go-undo ((sgf sgf)) (prev sgf))

(defmethod go-pass ((sgf sgf))
  (signal 'unsupported-back-end-command (list sgf :pass)))

(defmethod go-resign ((sgf sgf))
  (signal 'unsupported-back-end-command (list sgf :resign)))

(provide 'go-sgf)
