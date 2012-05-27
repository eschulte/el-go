;;; go-gtp.el --- GTP backend for go-trans

;; Copyright (C) 2008 2012 Eric Schulte <eric.schulte@gmx.com>

;; Author: Eric Schulte <eric.schulte@gmx.com>
;; Created: 2012-05-15
;; Version: 0.1
;; Keywords: game go sgf gtp gnugo

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

;; This file should be useful for translating between sgf and the GO
;; text protocol (GTP) see http://www.lysator.liu.se/~gunnar/gtp/.
;; The GMP command set may be implemented as an extension.

;; Code:
(require 'go-util)
(require 'go-trans)

(defun go-gtp-expand-color (turn)
  (case turn
    (:B "black")
    (:W "white")
    (t (error "gtp: unknown turn %S" turn))))

(defun go-gtp-char-to-num (char)
  (flet ((err () (error "go-gtp: invalid char %s" char)))
    (cond
     ((< char ?A)  (err))
     ((< char ?I)  (- char ?A))
     ((<= char ?T) (1- (- char ?A)))
     ((< char ?a)  (err))
     ((< char ?i)  (- char ?a))
     ((<= char ?t) (1- (- char ?a)))
     (t (err)))))

(defun go-gtp-num-to-char (num)
  (flet ((err () (error "go: invalid num %s" num)))
    (cond
     ((< num 1) (err))
     ((< num 9) (+ ?A (1- num)))
     (t         (+ ?A num)))))

(defun go-pos-to-gtp (pos)
  (format "%c%d" (go-gtp-num-to-char (1+ (car pos))) (1+ (cdr pos))))

(defun go-gtp-to-pos (color gtp)
  (cons color (cons :pos (cons (go-gtp-char-to-num (aref gtp 0))
                               (1- (parse-integer (substring gtp 1)))))))

(defun go-to-gtp-command (element)
  "Convert an go ELEMENT to a gtp command."
  (let ((key (car element))
	(val (cdr element)))
    (case key
      (:B       (format "black %s" (go-pos-to-gtp (aget (list val) :pos))))
      (:W       (format "white %s" (go-pos-to-gtp (aget (list val) :pos))))
      ((:SZ :S) (format "boardsize %s" val))
      (:KM      (format "komi %s" val))
      (t        nil))))


;;; Class and interface
(defclass gtp nil nil "Class for the GTP GO GO back end.")

(defgeneric gtp-command (back-end command)
  "Send gtp COMMAND to OBJECT and return any output.")

(defmethod go-size ((gtp gtp))
  (parse-integer (gtp-command gtp "query_boardsize")))

(defmethod set-go-size ((gtp gtp) size)
  (gtp-command gtp (format "boardsize %d" size)))

(defmethod go-name ((gtp gtp))
  (gtp-command gtp "name"))

(defmethod set-go-name ((gtp gtp) name)
  (signal 'unsupported-back-end-command (list gtp :set-name name)))

(defmethod go-move ((gtp gtp))
  (let ((color (go-color gtp)))
    (go-gtp-to-pos color
                   (case color
                     (:B (gtp-command gtp "genmove_black"))
                     (:W (gtp-command gtp "genmove_white"))))))

(defmethod set-go-move ((gtp gtp) move)
  (gtp-command gtp (go-to-gtp-command move)))

(defmethod go-labels ((gtp gtp))
  (signal 'unsupported-back-end-command (list gtp :labels)))

(defmethod set-go-labels ((gtp gtp) labels)
  (signal 'unsupported-back-end-command (list gtp :set-labels labels)))

(defmethod go-comment ((gtp gtp))
  (signal 'unsupported-back-end-command (list gtp :comment)))

(defmethod set-go-comment ((gtp gtp) comment)
  (signal 'unsupported-back-end-command (list gtp :set-comment comment)))

(defmethod go-alt ((gtp gtp))
  (signal 'unsupported-back-end-command (list gtp :alt)))

(defmethod set-go-alt ((gtp gtp) alt)
  (signal 'unsupported-back-end-command (list gtp :set-alt alt)))

(defmethod go-color ((gtp gtp))
  (case (condition-case err
            (intern (car (split-string (gtp-command gtp "last_move"))))
          (error 'white)) ('white :B) ('black :W)))

(defmethod set-go-color ((gtp gtp) color)
  (signal 'unsupported-back-end-command (list gtp :set-color color)))

;; non setf'able generic functions
(defmethod go-undo ((gtp gtp)) (gtp-command gtp "undo"))

(defmethod go-pass ((gtp gtp))
  (gtp-command gtp (format "%s pass" (go-gtp-expand-color (go-color gtp)))))

(defmethod go-resign ((gtp gtp))
  (gtp-command gtp (format "%s resign" (go-gtp-expand-color (go-color gtp)))))

(defmethod go-reset ((gtp gtp)) (gtp-command gtp "clear_board"))

(provide 'go-gtp)
