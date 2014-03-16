;;; gtp-pipe.el --- GTP backend through a pipe

;; Copyright (C) 2013  Free Software Foundation, Inc.

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

;;; Code:
(require 'go-api)
(require 'gtp)
(require 'comint)

(defvar *gtp-pipe-board* nil
  "Board associated with the current gtp pipe process.")

(defvar *gtp-pipe-last* nil
  "Last move of the current game.")

(defvar *gtp-pipe-inhibit* nil
  "Prevent infinite loops of commands.")

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
     (let ((color (go-re-cond (match-string 1 string)
                    ("black" :B)
                    ("white" :W)))
           (action (match-string 2 string)))
       (go-re-cond action
         ("^pass"   (let ((*gtp-pipe-inhibit* t)) (go-pass   *gtp-pipe-board*)))
         ("^resign" (let ((*gtp-pipe-inhibit* t)) (go-resign *gtp-pipe-board*)))
         (t (let ((move (gtp-to-pos color action)))
              (setf *gtp-pipe-last* move)
              (setf (go-move *gtp-pipe-board*) move))))))
    ("^genmove_\\(black\\|white\\)"
     (message "gtp-pipe: %s's turn" (match-string 1 string)))
    ("^last_move" (go-to-gtp-command *gtp-pipe-last*))
    ("^quit" (let ((*gtp-pipe-inhibit* t)) (go-quit *gtp-pipe-board*)))
    ("^undo" (let ((*gtp-pipe-inhibit* t)) (go-undo *gtp-pipe-board*)))
    ("^string \\(.*\\)$" (message "gtp-pipe: %S" (match-string 1 string)))
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
            (set (make-local-variable '*gtp-pipe-last*) nil)
            (set (make-local-variable '*gtp-pipe-inhibit*) nil)
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
    (unless *gtp-pipe-inhibit*
      (goto-char (process-mark (get-buffer-process (current-buffer))))
      (insert command)
      (comint-send-input))))

(defmethod go-comment ((gtp-pipe gtp-pipe))
  (signal 'unsupported-back-end-command (list gtp-pipe :comment)))

(defmethod set-go-comment ((gtp-pipe gtp-pipe) comment)
  (gtp-command gtp-pipe (format "string %s" comment)))

(defmethod go-color ((gtp-pipe gtp-pipe))
  (with-current-buffer (buffer gtp-pipe)
    (go-color *gtp-pipe-board*)))

(defmethod go-name ((gtp-pipe gtp-pipe)) "GTP pipe")
(defmethod go-size ((gtp-pipe gtp-pipe))
  (read-from-minibuffer "GTP board size: " nil nil 'read))

(defmethod go-quit ((gtp-pipe gtp-pipe))
  (gtp-command gtp-pipe "quit")
  (with-current-buffer (buffer gtp-pipe)
    (signal-process (get-buffer-process) 'KILL)))

(defmethod go-player-name ((gtp-pipe gtp-pipe) color) "GTP pipe")

(defmethod set-player-name ((gtp-pipe gtp-pipe) color name)
  (signal 'unsupported-back-end-command (list gtp-pipe :set-player-name name)))

(provide 'gtp-pipe)
;;; gtp-pipe.el ends here
