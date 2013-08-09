;;; gtp-pipe.el --- GTP backend through a pipe

;; Copyright (C) 2013 Eric Schulte <schulte.eric@gmail.com>

;; Author: Eric Schulte <schulte.eric@gmail.com>
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
(require 'go-api)
(require 'gtp)
(require 'comint)

(defvar *gtp-pipe-board* nil
  "Board associated with the current gtp pipe process.")

(defun gtp-pipe-start (command)
  "Connect a `gtp-pipe' instance to the process created by COMMAND.
Pass \"netcat -lp 6666\" as COMMAND to listen on a local port, or
pass \"netcat localhost 6666\" to connect to a listening local
port."
  (interactive "sgtp-pipe command: ")
  (pop-to-buffer (go-connect (make-instance 'gtp-pipe :command command))))

(defun gtp-pipe-process-filter (proc string)
  (go-re-cond string
    ("^\\(black\\|white\\) \\(.*\\)$"
     (setf (go-move *gtp-pipe-board*)
           (gtp-to-pos (go-re-cond (match-string 1 string)
                         ("black" :B)
                         ("white" :W))
                       (match-string 2 string))))
    ("^undo" (go-undo *gtp-pipe-board*))
    (t (message "gtp-pipe unknown command: %S" string))))


;;; Class and interface
(defclass gtp-pipe (gtp)
  ((buffer  :initarg :buffer  :accessor buffer)
   (command :initarg :command :accessor command)))

(defmethod go-connect ((gtp-pipe gtp-pipe))
  (setf (buffer gtp-pipe)
        (let* ((cmd-&-args (split-string (command gtp-pipe) " " 'omit-nulls))
               (buf (apply #'make-comint "gtp-pipe"
                           (car cmd-&-args) nil (cdr cmd-&-args))))
          (with-current-buffer buf
            (comint-mode)
            (set (make-local-variable '*gtp-pipe-board*)
                 (save-excursion
                   (make-instance 'board
                     :buffer (go-board gtp-pipe (make-instance 'sgf)))))
            (set-process-filter (get-buffer-process (current-buffer))
                                (make-go-insertion-filter
                                 #'gtp-pipe-process-filter)))
          buf)))

(defmethod gtp-command ((gtp-pipe gtp-pipe) command)
  (with-current-buffer (buffer gtp-pipe)
    (goto-char (process-mark (get-buffer-process (current-buffer))))
    (insert command)
    (comint-send-input)))

(defmethod go-name ((gtp-pipe gtp-pipe)) "GTP pipe")
(defmethod go-size ((gtp-pipe gtp-pipe))
  (read-from-minibuffer "GTP board size: " nil nil 'read))

(defmethod go-player-name ((gtp-pipe gtp-pipe) color) "GTP pipe")

(defmethod set-player-name ((gtp-pipe gtp-pipe) color name)
  (signal 'unsupported-back-end-command (list gtp-pipe :set-player-name name)))

(provide 'gtp-pipe)
