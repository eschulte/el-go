;;; igs.el --- IGS GO back-end

;; Copyright (C) 2012-2013  Free Software Foundation, Inc.

;; Author: Eric Schulte <schulte.eric@gmail.com>
;; Created: 2012-05-15
;; Version: 0.1
;; Keywords: game go sgf igs

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

;; http://www.pandanet.co.jp/English/commands/term/Summary.html

;; Code:
(require 'go-api)
(require 'list-buffer)

(defvar igs-ignore-shouts t
  "Ignore shouts on the IGS server.")

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

(defvar igs-server-ping-delay 300
  "Minimum time between pings to remind the IGS server we're still listening.")

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

(defvar *igs-instance* nil
  "IGS instance associated with the current buffer.")

(defvar *igs-time-last-sent* nil
  "Time stamp of the last command sent.
This is used to re-send messages to keep the IGS server from timing out.")

(defvar *igs-last-command* nil
  "Last command sent to the IGS process.")

(defvar *igs-games* nil
  "List holding the current games on the IGS server.")

(defvar *igs-current-game* nil
  "Number of the current IGS game (may change frequently).")


;;; Class and interface
(defclass igs ()
  ((buffer :initarg :buffer :accessor buffer :initform nil)
   ;; number of an observed IGS game
   (number :initarg :number :accessor number :initform nil)
   (active :initarg :active :accessor active :initform t)))

(defmethod go-connect ((igs igs)) (igs-connect igs))

(defmacro with-igs (igs &rest body)
  (declare (indent 1))
  `(with-current-buffer (buffer ,igs) ,@body))

(defmethod go-level ((igs igs))
  (signal 'unsupported-back-end-command (list igs :level)))

(defmethod set-go-level ((igs igs) level)
  (signal 'unsupported-back-end-command (list igs :set-level level)))

(defmethod go-size ((igs igs))
  (with-igs igs (aget (igs-current-game) :size)))

(defmethod set-go-size ((igs igs) size)
  (signal 'unsupported-back-end-command (list igs :set-size size)))

(defmethod go-name ((igs igs))
  (with-igs igs (let ((game (igs-current-game)))
                  (format "%s(%s) vs %s(%s)"
                          (aget game :white-name)
                          (aget game :white-rank)
                          (aget game :black-name)
                          (aget game :black-rank)))))

(defmethod set-go-name ((igs igs) name)
  (signal 'unsupported-back-end-command (list igs :set-name name)))

(defmethod go-move ((igs igs))
  (signal 'unsupported-back-end-command (list igs :move)))

(defmethod set-go-move ((igs igs) move)
  (signal 'unsupported-back-end-command (list igs :set-move move)))

(defmethod go-labels ((igs igs))
  (signal 'unsupported-back-end-command (list igs :labels)))

(defmethod set-go-labels ((igs igs) labels)
  (signal 'unsupported-back-end-command (list igs :set-labels labels)))

(defmethod go-comment ((igs igs))
  (signal 'unsupported-back-end-command (list igs :comment)))

(defmethod set-go-comment ((igs igs) comment)
  (signal 'unsupported-back-end-command (list igs :set-comment comment)))

(defmethod go-alt ((igs igs))
  (signal 'unsupported-back-end-command (list igs :alt)))

(defmethod set-go-alt ((igs igs) alt)
  (signal 'unsupported-back-end-command (list igs :set-alt alt)))

(defmethod go-color ((igs igs))
  (signal 'unsupported-back-end-command (list igs :color)))

(defmethod set-go-color ((igs igs) color)
  (signal 'unsupported-back-end-command (list igs :set-color color)))

(defmethod go-player-name ((igs igs) color)
  (with-igs igs (aget (igs-current-game)
                      (case color
                        (:W :white-name)
                        (:B :black-name)))))

(defmethod set-go-player-name ((igs igs) color name)
  (signal 'unsupported-back-end-command (list igs :set-player-name color name)))

(defmethod go-player-time ((igs igs) color)
  (signal 'unsupported-back-end-command (list igs :player-time color)))

(defmethod set-go-player-time ((igs igs) color time)
  (signal 'unsupported-back-end-command (list igs :set-player-time color time)))

;; non setf'able generic functions
(defmethod go-undo ((igs igs))
  (signal 'unsupported-back-end-command (list igs :undo)))

(defmethod go-pass ((igs igs))
  (signal 'unsupported-back-end-command (list igs :pass)))

(defmethod go-resign ((igs igs))
  (signal 'unsupported-back-end-command (list igs :resign)))

(defmethod go-reset ((igs igs))
  (signal 'unsupported-back-end-command (list igs :reset)))

(defmethod go-quit ((igs igs))
  (with-igs igs
    (if (number igs)
        (progn
          ;; TOOD: ensure still on our server-side observation list
          ;;       (e.g., hasn't been removed after a resignation)
          (when (active igs)
            (igs-send (format "observe %d" (number igs))))
          (setf (number igs) nil))
      (igs-send "quit"))))

(defmethod go-score ((igs igs))
  (signal 'unsupported-back-end-command (list igs :score)))

(defmethod go-territory ((igs igs))
  (signal 'unsupported-back-end-command (list igs :territory)))

(defmethod go-dead ((igs igs))
  (signal 'unsupported-back-end-command (list igs :dead)))

(defmacro igs-w-proc (proc &rest body)
  (declare (indent 1))
  `(with-current-buffer (process-buffer proc) ,@body))
(def-edebug-spec igs-w-proc (form body))

(defun igs-send (command)
  "Send string COMMAND to the IGS process in the current buffer."
  (goto-char (process-mark (get-buffer-process (current-buffer))))
  (insert command)
  (setq *igs-time-last-sent* (current-time))
  (setq *igs-last-command* (and (string-match "^\\([^ ]*\\)" command)
                                (match-string 1 command)))
  (comint-send-input))

(defun igs-process-filter (proc string)
  (when (string-match "^\\([[:digit:]]+\\) \\(.+\\)$" string)
    (let* ((number  (read (match-string 1 string)))
           (type    (car (rassoc number igs-message-types)))
           (content (match-string 2 string)))
      (case type
        (:prompt
         (go-re-cond (or *igs-last-command* "")
           ("^games" (igs-list-games *igs-instance* *igs-games*))
           (t nil))
         (setq *igs-last-command* nil))
        (:info
         (go-re-cond content
           ;; Game NN: name1 vs name2 has adjourned.
           ("^Game \\([0-9]*\\): .*adjourned.$"
            (igs-handle-adjournment (match-string 1 content)))
           ;; {Game NN: name1 vs name2 : color resigns.}
           ("^{Game \\([0-9]*\\): \\(Black\\|White\\) resigns.}$"
            (igs-handle-resignation (go-re-cond (match-string 2 content)
                                      ("black" :black)
                                      ("white" :white))))
           (t (unless (string= content "yes")
                (message "igs-info: %s" content)))))
        (:games  (igs-w-proc proc (igs-handle-game content)))
        (:move   (igs-w-proc proc (igs-handle-move content)))
        (:kibitz (message "igs-kibitz: %s" content))
        (:tell   (igs-handle-tell content))
        (:beep   nil)
        (:shout  (unless igs-ignore-shouts (igs-handle-shout content)))
        (t       (message "igs-unknown: [%s]%s" type content)))
      (when (and *igs-time-last-sent*
                 (> (time-to-seconds (time-since *igs-time-last-sent*))
                    igs-server-ping-delay))
        (igs-send "ayt")))))

(defun igs-connect (igs)
  "Open a connection to `igs-server'."
  (cl-flet ((wait (prompt)
                  (message "IGS waiting for %S..." prompt)
                  (while (and (goto-char (or comint-last-input-end (point-min)))
                              (not (re-search-forward prompt nil t)))
                    (accept-process-output proc))))
    (let ((buffer (apply 'make-comint
                         igs-process-name
                         igs-telnet-command nil
                         (list igs-server (number-to-string igs-port)))))
      (setf (buffer igs) buffer)
      (with-current-buffer buffer
        (comint-mode)
        (set (make-local-variable '*igs-instance*) igs)
        (set (make-local-variable '*igs-last-command*) "")
        (set (make-local-variable '*igs-games*) nil)
        (set (make-local-variable '*igs-current-game*) nil)
        (set (make-local-variable '*go-partial-line*) nil)
        (set (make-local-variable '*igs-time-last-sent*) (current-time))
        (let ((proc (get-buffer-process (current-buffer))))
          (wait "^Login:")
          (goto-char (process-mark proc))
          (igs-send igs-username)
          (wait "^\#> ")
          (igs-toggle "client" t)
          (set-process-filter
           proc (make-go-insertion-filter #'igs-process-filter))))
      buffer)))

(defun igs-toggle (setting value)
  (igs-send (format "toggle %s %s" setting (if value "true" "false"))))

(defun igs-observe (game) (igs-send (format "observe %s" game)))

(defun igs-list-games (instance games)
  (lexical-let ((instance instance))
    (list-buffer-create
     "*igs-game-list*"
     (cl-mapcar #'cons
                (mapcar #'car games)
                (mapcar (curry #'mapcar #'cdr) (mapcar #'cdr games)))
     '("#" "white" "rk" "black" "rk" "move" "size" "H" "Komi" "by" "fr" "#")
     (lambda (row col)
       (let ((id (car (nth row *buffer-list*))))
         (with-igs instance (igs-observe id))))
     (lambda (row col)
       (message "refreshing games list...")
       (igs-get-games instance)))))


;;; Specific handlers
(defvar igs-player-name-re
  "[[:alpha:][:digit:]]+"
  "Regular expression used to match igs player name.")

(defvar igs-player-rating-re
  "[[:digit:]]+[kd]\\*?"
  "Regular expression used to match igs player rating.")

(defvar igs-player-game-info-re "([-[:digit:]]+ [-[:digit:]]+ [-[:digit:]]+)"
  "Regular expression used to match igs player game info.")

(defvar igs-player-re
  (format "\\(%s\\) +\\[ *\\(%s\\)\\]" igs-player-name-re igs-player-rating-re)
  "Regular expression used to parse igs player name and rating.")

(defvar igs-game-re
  (format
   "\\[\\([[:digit:]]+\\)\\] +%s +vs. +%s +\\((.+)\\) \\((.+)\\)[[:space:]]*$"
   igs-player-re igs-player-re)
  "Regular expression used to parse igs game listings.")

(defvar igs-move-piece-re
  "[[:digit:]]+(\\([WB]\\)): \\([[:alpha:]][[:digit:]]+\\)"
  "Regular expression used to match an IGS move.")

(defvar igs-move-time-re "TIME")

(defvar igs-move-props-re "GAMEPROPS")

(defvar igs-move-game-re
  (format "Game \\([[:digit:]]+\\) I: \\(%s\\) \\(%s\\) vs \\(%s\\) \\(%s\\)"
          igs-player-name-re igs-player-game-info-re
          igs-player-name-re igs-player-game-info-re)
  "Regular expression used to match Game updates.")

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
            *igs-games*)
      ;; update the game list buffer
      (when (get-buffer "*igs-game-list*")
        (save-excursion
          (set-buffer (get-buffer "*igs-game-list*"))
          (list-buffer-refresh))))))

(defun igs-handle-adjournment (number-string)
  (if (aget (igs-current-game) :board)
      (with-current-buffer (buffer (aget (igs-current-game) :board))
        (with-backends backend
          (when (equal (class-of backend) 'igs)
            (setf (active backend) nil))))
    (error "igs-handle-adjournment: no board!")))

(defun igs-handle-resignation (color)
  (if (aget (igs-current-game) :board)
      (progn
        (go-resign (aget (igs-current-game) :board))
        (with-current-buffer (buffer (aget (igs-current-game) :board))
          (with-backends backend
            (when (equal (class-of backend) 'igs)
              (setf (active backend) nil)))))
    (error "igs-handle-adjournment: no board!")))

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

(defun igs-handle-tell (string)
  (unless (string-match (format "\\*\\(%s\\)\\*: \\(.*\\)$" igs-player-name-re)
                        string)
    (error "igs: malformed tell string %S" string))
  ;; TODO: keep a message buffer for each user in which conversations
  ;;       may be saved... during games store messages as SGF comments.
  (message "igs[%s]: %s" (match-string 1 string) (match-string 2 string)))

(defun igs-handle-shout (string)
  (unless (string-match "^\\([^:]*\\): \\(.*\\)$" string)
    (error "igs: malformed shout string %S" string))
  (message "IGS[%s]: %s" (match-string 1 string) (match-string 2 string)))

(defun igs-apply-move (move)
  (if (aget (igs-current-game) :board)
      (setf (go-move (aget (igs-current-game) :board)) move)
    (message "igs-apply-move: no board!")))

(defun igs-register-game (number)
  (setq *igs-current-game* number)
  (unless (aget (igs-current-game) :board)
    (setf (aget (igs-current-game) :board)
          (save-excursion
            (setf (number *igs-instance*) number)
            (make-instance 'board
              :buffer (go-board *igs-instance*
                                (make-instance 'sgf)))))
    (when (aget (igs-current-game) :board)
      (igs-send (format "moves %s" number)))))

(defun igs-update-game-info (info)
  (let ((color (car info))
        (name (cadr info))
        (other (cddr info)))
    ;; (message "[%s] %s: %s" color name other)
    ))

(defun igs-handle-move (move-string)
  (go-re-cond move-string
    (igs-move-piece-re (igs-apply-move
                        (igs-to-pos (match-string 1 move-string)
                                    (match-string 2 move-string))))
    (igs-move-time-re  nil)
    (igs-move-props-re nil)
    (igs-move-game-re
     (let ((number (read (match-string 1 move-string)))
           (white-info (cons (match-string 2 move-string)
                             (read (match-string 3 move-string))))
           (black-info (cons (match-string 4 move-string)
                             (read (match-string 5 move-string)))))
       (igs-register-game number)
       (igs-update-game-info (cons :W white-info))
       (igs-update-game-info (cons :B black-info))))))


;;; Interface
;;
;; If we find another backend providing game lists and observations
;; then this could be generalized to an interface.
(defun igs-start (&optional name)
  "Connect to an IGS server and return the `igs' instance."
  (interactive)
  (set-buffer (get-buffer-create (or name "*igs*")))
  (if (get-buffer-process (current-buffer))
      *igs-instance*
    (let ((*igs* (make-instance 'igs)))
      (igs-connect *igs*)
      *igs*)))

(defun igs-get-games (&optional instance)
  "List the games of the igs instance."
  (interactive)
  (set-buffer (buffer (or instance (igs-start))))
  (setf *igs-games* nil)
  (igs-send "games"))

(provide 'igs)
;;; igs.el ends here
