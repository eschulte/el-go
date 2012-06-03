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

(defvar *igs-ready* nil
  "Indicates if the IGS server is waiting for input.")

(defvar *igs-games* nil
  "List holding the current games on the IGS server.")

(defvar *igs-current-game* nil
  "Number of the current IGS game (may change frequently).")

(defmacro igs-w-proc (proc &rest body)
  (declare (indent 1))
  `(with-current-buffer (process-buffer proc) ,@body))
(def-edebug-spec igs-w-proc (form body))

(defun igs-filter-process (proc string)
  (when (string-match "^\\([[:digit:]]+\\) \\(.+\\)$" string)
    (let* ((number  (read (match-string 1 string)))
           (type    (car (rassoc number igs-message-types)))
           (content (match-string 2 string)))
      (case type
        (:prompt (igs-w-proc proc (setq *igs-ready* t)))
        (:info   (message "igs-info: %s" content))
        (:games  (igs-w-proc proc (igs-handle-game content)))
        (:move   (igs-w-proc proc (igs-handle-move content)))))))

(defun igs-insertion-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
	(goto-char (process-mark proc))
        (insert string)
        (set-marker (process-mark proc) (point))
        (mapc (lambda (s) (igs-filter-process proc s))
              (split-string string "[\n\r]")))
      (when moving (goto-char (process-mark proc))))))

(defun igs-connect ()
  "Open a connection to `igs-server'."
  (interactive)
  (flet ((wait (prompt)
               (while (and (goto-char (or comint-last-input-end (point-min)))
                           (not (re-search-forward prompt nil t)))
                 (accept-process-output proc))))
    (let ((buffer (apply 'make-comint
                         igs-process-name
                         igs-telnet-command nil
                         (list igs-server (number-to-string igs-port)))))
      (with-current-buffer buffer
        (comint-mode)
        (set (make-local-variable '*igs-ready*) nil)
        (set (make-local-variable '*igs-games*) nil)
        (set (make-local-variable '*igs-current-game*) nil)
        (let ((proc (get-buffer-process (current-buffer))))
          (wait "^Login:")
          (goto-char (process-mark proc))
          (insert igs-username)
          (comint-send-input)
          (wait "^\#> ")
          (igs-toggle "client" t)
          (set-process-filter proc 'igs-insertion-filter)
          buffer)))))

(defun igs-toggle (setting value)
  (insert (format "toggle %s %s" setting (if value "true" "false")))
  (comint-send-input))

(defun igs-observe (&optional game)
  (interactive)
  (let ((game (or game (read (org-icompleting-read
                              "game: "
                              (mapcar #'number-to-string
                                      (mapcar #'car *igs-games*)))))))
    (insert (format "observe %s" game))
    (comint-send-input)))

(defun igs-games ()
  (interactive)
  (setf *igs-games* nil)
  (insert "games")
  (comint-send-input))

(defun igs-game-list (igs)
  (let (games)
    (with-current-buffer (buffer igs)
      (setq games *igs-games*))
    (let* ((my-games (copy-seq games))
           (list-buf (get-buffer-create "*igs-game-list*")))
      (with-current-buffer (pop-to-buffer list-buf)
        (delete-region (point-min) (point-max))
        (org-mode)
        (insert (concat (orgtbl-to-orgtbl
                         (mapcar (lambda (game)
                                   (cons (car game)
                                         (mapcar #'cdr
                                                 (assq-delete-all
                                                  :board (cdr game)))))
                                 my-games)
                         '(:fmt (lambda (cell) (format "%s" cell)))) "\n"))
        (goto-char (point-min))
        (org-table-align)))))


;;; Specific handlers
(defvar igs-player-re
  "\\([[:alpha:][:digit:]]+\\) +\\[ *\\([[:digit:]]+[kd]\\*\\)\\]"
  "Regular expression used to parse igs player name and rating.")

(defvar igs-game-re
  (format "\\[\\([[:digit:]]+\\)\\] +%s +vs. +%s +\\((.+)\\) \\((.+)\\)$"
          igs-player-re igs-player-re)
  "Regular expression used to parse igs game listings.")

(defvar igs-move-piece-re
  "[[:digit:]]+(\\([WB]\\)): \\([[:alpha:][:digit:]]+\\)$"
  "Regular expression used to match an IGS move.")

(defvar igs-move-time-re "TIME")

(defvar igs-move-props-re "GAMEPROPS")

(defvar igs-move-game-re "Game \\([[:digit:]]+\\)")

(defmacro igs-re-cond (string &rest body)
  (declare (indent 1))
  `(cond ,@(mapcar
            (lambda (part)
              (cons (if (or (keywordp (car part)))
                        (car part)
                      `(string-match ,(car part) ,string))
                    (cdr part)))
            body)))
(def-edebug-spec igs-re-cond (form body))

(defun igs-handle-game (game-string)
  ;; [##] white name [ rk ] black name [ rk ] (Move size H Komi BY FR) (###)
  (when (string-match igs-game-re game-string)
    (let* ((num        (match-string 1 game-string))
           (white-name (match-string 2 game-string))
           (white-rank (match-string 3 game-string))
           (black-name (match-string 4 game-string))
           (black-rank (match-string 5 game-string))
           (other1     (read (match-string 6 game-string)))
           (other2     (read (match-string 7 game-string))))
      (push `(,(read num)
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
              (:other      . ,(car other2)))
            *igs-games*))))

(defun igs-to-pos (color igs)
  (cons (make-keyword color)
        (cons :pos
              (cons (char-to-num (aref igs 0))
                    (1- (read (substring igs 1)))))))

(defun igs-current-game ()
  (aget *igs-games* *igs-current-game*))

(defun set-igs-current-game (new)
  (setf (aget *igs-games* *igs-current-game*) new))

(defsetf igs-current-game set-igs-current-game)

(defun igs-apply-move (move)
  (if (aget (igs-current-game) :board)
      (setf (go-move (aget (igs-current-game) :board)) move)
    (message "igs-apply-move: no board!")))

(defun igs-register-game (number)
  (setq *igs-current-game* number)
  (unless (aget (igs-current-game) :board)
    (let ((sgf (make-instance 'sgf)))
      (setf (go-size sgf) (aget (igs-current-game) :size))
      (setf (go-name sgf) (format "igs-%d" number))
      (setf (aget (igs-current-game) :board)
            (save-excursion (make-instance 'board
                              :buffer (go-board sgf))))
      (insert (format "moves %s" number))
      (comint-send-input))))

(defun igs-handle-move (move-string)
  (igs-re-cond move-string
    (igs-move-piece-re (igs-apply-move
                        (igs-to-pos (match-string 1 move-string)
                                    (match-string 2 move-string))))
    (igs-move-time-re  nil)
    (igs-move-props-re nil)
    (igs-move-game-re  (igs-register-game
                        (read (match-string 1 move-string))))))


;;; Class and interface
(defclass igs ()
  ((buffer :initarg :buffer :accessor buffer :initform nil)))

(provide 'igs)
