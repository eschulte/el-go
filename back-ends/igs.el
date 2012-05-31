;;; igs.el --- IGS GO back-end

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

;; http://www.pandanet.co.jp/English/commands/term/Summary.html

;; Code:
(require 'go)

(defvar igs-telnet-command "telnet"
  "Telnet command used by igs.")

(defvar igs-server "igs.joyjoy.net"
  "Address of the IGS server.")

(defvar igs-port 6969
  "Port to use when connecting to an IGS server.")

(defvar igs-username "guest"
  "User name to use when connecting to an IGS server.")

(defvar igs-process-name "igs"
  "Name for the igs process.")

(defvar igs-message-types
  '((:unknown   . 0)
    (:automat   . 35)   ;; Automatch announcement
    (:autoask   . 36)   ;; Automatch accept
    (:choices   . 38)   ;; game choices
    (:clivrfy   . 41)   ;; Client verify message
    (:beep      . 2)    ;; \7 telnet
    (:board     . 3)    ;; Board being drawn
    (:down      . 4)    ;; The server is going down
    (:error     . 5)    ;; An error reported
    (:fil       . 6)    ;; File being sent
    (:games     . 7)    ;; Games listing
    (:help      . 8)    ;; Help file
    (:info      . 9)    ;; Generic info
    (:last      . 10)   ;; Last command
    (:kibitz    . 11)   ;; Kibitz strings
    (:load      . 12)   ;; Loading a game
    (:look_m    . 13)   ;; Look
    (:message   . 14)   ;; Message listing
    (:move      . 15)   ;; Move #:(B) A1
    (:observe   . 16)   ;; Observe report
    (:prompt    . 1)    ;; A Prompt (never)
    (:refresh   . 17)   ;; Refresh of a board
    (:saved     . 18)   ;; Stored command
    (:say       . 19)   ;; Say string
    (:score_m   . 20)   ;; Score report
    (:sgf_m     . 34)   ;; SGF variation
    (:shout     . 21)   ;; Shout string
    (:show      . 29)   ;; Shout string
    (:status    . 22)   ;; Current Game status
    (:stored    . 23)   ;; Stored games
    (:teach     . 33)   ;; teaching game
    (:tell      . 24)   ;; Tell string
    (:dot       . 40)   ;; your . string
    (:thist     . 25)   ;; Thist report
    (:tim       . 26)   ;; times command
    (:trans     . 30)   ;; Translation info
    (:ttt_board . 37)   ;; tic tac toe
    (:who       . 27)   ;; who command
    (:undo      . 28)   ;; Undo report
    (:user      . 42)   ;; Long user report
    (:version   . 39)   ;; IGS Version
    (:yell      . 32))) ;; Channel yelling

(defvar igs-player-re
  "\\([[:alpha:][:digit:]]+\\) +\\[ *\\([[:digit:]]+[kd]\\*\\)\\]"
  "Regular expression used to parse igs player name and rating.")

(defvar igs-game-re
  (format "\\[\\([[:digit:]]+\\)\\] +%s +vs. +%s +\\((.+)\\) \\((.+)\\)$"
          igs-player-re igs-player-re)
  "Regular expression used to parse igs game listings.")

(defvar *igs-ready* nil
  "Indicates if the IGS server is waiting for input.")

(defvar *igs-games* nil
  "List holding the current games on the IGS server.")

(defun igs-toggle (setting value)
  (insert (format "toggle %s %s" setting (if value "true" "false")))
  (comint-send-input))

(defun igs-filter-process (string)
  (unless (string-match "^\\([[:digit:]]+\\) \\(.+\\)$" string)
    (error "igs: malformed response %S" string))
  (let* ((number  (match-string 1 string))
         (content (match-string 2 string)))
    (case (car (rassoc number igs-message-types))
      (:prompt (set *igs-ready* t))
      (:info   (message "igs-info: %s" content))
      (:games  (push (igs-parse-game-string content) *igs-games*)))))

(defun igs-insertion-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
	(goto-char (process-mark proc))
        (insert string)
        (set-marker (process-mark proc) (point))
        (igs-filter-process string))
      (when moving (goto-char (process-mark proc))))))

(defun igs-connect ()
  "Open a connection to `igs-server'."
  (interactive)
  (flet ((wait (prompt)
               (while (and (goto-char (or comint-last-input-end (point-min)))
                           (not (re-search-forward (prompt) nil t)))
                 (accept-process-output proc))))
    (let ((buffer (apply 'make-comint
                         igs-process-name
                         igs-telnet-command nil
                         (list igs-server (number-to-string igs-port)))))
      (with-current-buffer buffer
        (comint-mode)
        (set (make-local-variable '*igs-ready*) nil)
        (set (make-local-variable '*igs-games*) nil)
        (let ((proc (get-buffer-process (current-buffer))))
          (wait "^Login:")
          (goto-char (process-mark proc))
          (insert igs-username)
          (comint-send-input)
          (wait "^\#> ")
          (igs-toggle "client" t)
          (set-process-filter proc 'igs-insertion-filter)
          buffer)))))

(defun igs-last-output (igs)
  (with-current-buffer (buffer igs)
    (comint-show-output)
    (org-babel-clean-text-properties
     (buffer-substring (+ 2 (point)) (- (point-max) 2)))))

(defun igs-command-to-string (igs command)
  "Send command to an igs connection and return the results as a string"
  (interactive "sigs command: ")
  (with-current-buffer (buffer igs)
    (goto-char (process-mark (get-buffer-process (current-buffer))))
    (insert command)
    (comint-send-input))
  (igs-wait-for-output igs)
  (igs-last-output igs))

(defun igs-parse-game-string (game-string)
  ;; [##] white name [ rk ] black name [ rk ] (Move size H Komi BY FR) (###)
  (when (string-match igs-game-re game-string)
    (let* ((num        (match-string 1 game-string))
           (white-name (match-string 2 game-string))
           (white-rank (match-string 3 game-string))
           (black-name (match-string 4 game-string))
           (black-rank (match-string 5 game-string))
           (other1     (read (match-string 6 game-string)))
           (other2     (read (match-string 7 game-string))))
      `((:number     . ,(read num))
        (:white-name . ,white-name)
        (:white-rank . ,white-rank)
        (:black-name . ,black-name)
        (:black-rank . ,black-rank)
        (:move       . ,(nth 0 other1))
        (:size       . ,(nth 1 other1))
        (:h          . ,(nth 2 other1))
        (:komi       . ,(nth 3 other1))
        (:by         . ,(nth 4 other1))
        (:fr         . ,(nth 5 other1))
        (:other      . ,(car other2))))))

(defun igs-games (igs)
  (let ((games-str (igs-command-to-string igs "games")))
    (delete nil
            (mapcar #'igs-parse-game-string
                    (cdr (split-string games-str "[\n\r]"))))))


;;; Class and interface
(defclass igs ()
  ((buffer :initarg :buffer :accessor buffer :initform nil)))

(provide 'igs)
