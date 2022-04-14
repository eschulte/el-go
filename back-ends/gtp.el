;;; gtp.el --- GTP GO back-end

;; Copyright (C) 2008 2012  Free Software Foundation, Inc.

;; Author: Eric Schulte <schulte.eric@gmail.com>
;; Created: 2012-05-15
;; Version: 0.1
;; Keywords: game go sgf gtp gnugo

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

;; Commentary:

;; This file should be useful for translating between sgf and the GO
;; text protocol (GTP) see http://www.lysator.liu.se/~gunnar/gtp/.
;; The GMP command set may be implemented as an extension.
;;
;; see http://www.lysator.liu.se/~gunnar/gtp/gtp2-spec-draft2/gtp2-spec.html
;;
;; The following commands are required by GTP
;; - protocol_version
;; - name
;; - version
;; - known_command
;; - list_commands
;; - quit
;; - boardsize
;; - clear_board
;; - komi
;; - play
;; - genmove

;; Code:
(require 'go-api)

(defun gtp-expand-color (turn)
  (case turn
    (:B "black")
    (:W "white")
    (t (error "gtp: unknown turn %S" turn))))

(defun go-pos-to-gtp (pos)
  (format "%c%d" (num-to-char (1+ (car pos))) (1+ (cdr pos))))

(defun gtp-to-pos (color gtp)
  (cons color (cons :pos (cons (char-to-num (aref gtp 0))
                               (1- (read (substring gtp 1)))))))

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

(defun gtp-territory (gtp color)
  (let ((output (ecase color
                  (:B (gtp-command gtp "final_status_list black_territory"))
                  (:W (gtp-command gtp "final_status_list white_territory")))))
    (mapcar (lambda (gtp-point) (gtp-to-pos color gtp-point))
            (mapcar #'symbol-name
                    (read (format "(%s)" output))))))


;;; Class and interface
(defclass gtp nil nil "Class for the GTP GO GO back end.")

(cl-defgeneric gtp-command (back-end command)
  "Send gtp COMMAND to OBJECT and return any output.")

(cl-defmethod go-size ((gtp gtp))
  (read (gtp-command gtp "query_boardsize")))

(cl-defmethod set-go-size ((gtp gtp) size)
  (gtp-command gtp (format "boardsize %d" size)))

(cl-defmethod go-level ((gtp gtp))
  (signal 'unsupported-back-end-command (list gtp :go-level)))

(cl-defmethod set-go-level ((gtp gtp) level)
  (gtp-command gtp (format "level %d" level)))

(cl-defmethod go-name ((gtp gtp))
  (gtp-command gtp "name"))

(cl-defmethod set-go-name ((gtp gtp) name)
  (signal 'unsupported-back-end-command (list gtp :set-name name)))

(cl-defmethod go-move ((gtp gtp))
  (let* ((color (go-color gtp))
         (move (case color
                 (:B (gtp-command gtp "genmove_black"))
                 (:W (gtp-command gtp "genmove_white")))))
    (if (string= move "PASS")
        :pass
      (gtp-to-pos color move))))

(cl-defmethod set-go-move ((gtp gtp) move)
  (gtp-command gtp (go-to-gtp-command move)))

(cl-defmethod go-labels ((gtp gtp))
  (signal 'unsupported-back-end-command (list gtp :labels)))

(cl-defmethod set-go-labels ((gtp gtp) labels)
  (signal 'unsupported-back-end-command (list gtp :set-labels labels)))

(cl-defmethod go-comment ((gtp gtp))
  (signal 'unsupported-back-end-command (list gtp :comment)))

(cl-defmethod set-go-comment ((gtp gtp) comment)
  (signal 'unsupported-back-end-command (list gtp :set-comment comment)))

(cl-defmethod go-alt ((gtp gtp))
  (signal 'unsupported-back-end-command (list gtp :alt)))

(cl-defmethod set-go-alt ((gtp gtp) alt)
  (signal 'unsupported-back-end-command (list gtp :set-alt alt)))

(cl-defmethod go-color ((gtp gtp))
  (case (condition-case err
            (intern (car (split-string (gtp-command gtp "last_move"))))
          (error 'white)) ('white :B) ('black :W)))

(cl-defmethod set-go-color ((gtp gtp) color)
  (signal 'unsupported-back-end-command (list gtp :set-color color)))

;; non setf'able generic functions
(cl-defmethod go-undo ((gtp gtp)) (gtp-command gtp "undo"))

(cl-defmethod go-pass ((gtp gtp))
  (gtp-command gtp (format "%s pass" (gtp-expand-color (go-color gtp)))))

(cl-defmethod go-resign ((gtp gtp))
  (gtp-command gtp (format "%s resign" (gtp-expand-color (go-color gtp)))))

(cl-defmethod go-reset ((gtp gtp)) (gtp-command gtp "clear_board"))

(cl-defmethod go-quit ((gtp gtp)) (gtp-command gtp "quit"))

(cl-defmethod go-score ((gtp gtp)) (gtp-command gtp "final_score"))

(cl-defmethod go-territory ((gtp gtp))
  (append (gtp-territory gtp :B) (gtp-territory gtp :W)))

(cl-defmethod go-dead ((gtp gtp))
  (signal 'unsupported-back-end-command (list gtp :dead)))

(provide 'gtp)
;;; gtp.el ends here
