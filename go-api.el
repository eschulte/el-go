;;; go-api.el --- a uniform API for communication between GO back-ends

;; Copyright (C) 2012  Free Software Foundation, Inc.

;; Author: Eric Schulte <schulte.eric@gmail.com>
;; Created: 2012-05-15
;; Version: 0.1
;; Keywords: game go sgf

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

;;; Commentary:

;; A board-based interface to GO games which may be connected to a
;; number of GO back-ends through a generic API.  To play a game of GO
;; against the gnugo back-end run `play-go'.  Current back-ends
;; include the following.
;; - the SGF format
;; - the Go Text Protocol (GTP)
;; - TODO: the IGS protocol

;;; Code:
(require 'go-util)
(require 'eieio)

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

;; setf'able back-end access
(defgeneric-w-setf go-size    "Access BACK-END size.")
(defgeneric-w-setf go-level   "Access level of BACK-END.")
(defgeneric-w-setf go-name    "Access BACK-END name.")
(defgeneric-w-setf go-move    "Access current BACK-END move.")
(defgeneric-w-setf go-labels  "Access current BACK-END labels.")
(defgeneric-w-setf go-comment "Access current BACK-END comment.")
(defgeneric-w-setf go-alt     "Access current BACK-END alternative move.")
(defgeneric-w-setf go-color   "Access current BACK-END turn color.")
(defgeneric-w-setf go-player-name "Access current BACK-END player name.")
(defgeneric-w-setf go-player-time "Access current BACK-END player time.")
(defgeneric-w-setf
  go-player-prisoners         "Access current BACK-END player prisoners.")

;; sending messages to the back-end
(defgeneric go-connect (back-end) "Connect to BACK-END.")
(defgeneric go-undo   (back-end) "Send undo to BACK-END.")
(defgeneric go-pass   (back-end) "Send pass to BACK-END.")
(defgeneric go-resign (back-end) "Send resign to BACK-END.")
(defgeneric go-reset  (back-end) "Send reset to BACK-END.")
(defgeneric go-quit   (back-end) "Quit the BACK-END.")
(defgeneric go-score  (back-end) "Ask BACK-END to report the score.")
(defgeneric go-territory (back-end) "Ask BACK-END to report the territory.")
(defgeneric go-dead (back-end) "Ask BACK-END to dead stones.")

(provide 'go-api)
;;; go-api.el ends here
