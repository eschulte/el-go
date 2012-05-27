;;; go.el --- Play GO, translate and transfer between GO back ends

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

;;; Commentary:

;; An API for transferring GO moves and data between a number of GO
;; back ends including the following.
;; - the SGF format
;; - the Go Text Protocol (GTP)
;; - the IGS protocol

;;; Code:
(require 'go-util)
(require 'eieio)

(declare-function go-board "go-board" (back-end &rest trackers))
(declare-function gnugo-start-process "gnugo.el" (&rest options))

(put 'unsupported-back-end-command
     'error-conditions
     '(error unsupported-back-end-command))

(defmacro ignoring-unsupported (&rest body)
  `(condition-case err ,@body
     (unsupported-back-end-command nil)))

(defmacro defgeneric-w-setf (name doc)
  (let ((set-name (intern (concat "set-" (symbol-name name)))))
    `(progn
       (defgeneric ,name     (back-end) ,doc)
       (defgeneric ,set-name (back-end new))
       (defsetf ,name ,set-name))))

(defun play-go (&optional level)
  "Play a game of GO against gnugo.
Optional argument LEVEL specifies gnugo's level of play."
  (interactive "P")
  (let ((*autoplay* t))
    (go-board
     (make-instance 'gnugo
       :buffer (apply #'gnugo-start-process
                      (when level
                        (list "--level" (number-to-string level))))))))

;; setf'able back-end access
(defgeneric-w-setf go-size    "Access BACK-END size.")
(defgeneric-w-setf go-name    "Access BACK-END name.")
(defgeneric-w-setf go-move    "Access current BACK-END move.")
(defgeneric-w-setf go-labels  "Access current BACK-END labels.")
(defgeneric-w-setf go-comment "Access current BACK-END comment.")
(defgeneric-w-setf go-alt     "Access current BACK-END alternative move.")
(defgeneric-w-setf go-color   "Access current BACK-END turn color.")

;; sending messages to the back-end
(defgeneric go-undo   (back-end) "Send undo to BACK-END.")
(defgeneric go-pass   (back-end) "Send pass to BACK-END.")
(defgeneric go-resign (back-end) "Send resign to BACK-END.")
(defgeneric go-reset  (back-end) "Send reset to BACK-END.")

(provide 'go)
