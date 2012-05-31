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

(defvar igs-process-name "igs"
  "Name for the igs process.")

(defun igs-connect ()
  "Open a connection to `igs-server'."
  (interactive)
  (let ((buffer (apply 'make-comint
                       igs-process-name
                       igs-telnet-command nil
                       (list igs-server (number-to-string igs-port)))))
    (with-current-buffer buffer (comint-mode))
    buffer))

(defun igs-wait-for-output (igs)
  (with-current-buffer (buffer igs)
    (while  (progn
	     (goto-char comint-last-input-end)
	     (not (re-search-forward "^\#> " nil t)))
      (accept-process-output (get-buffer-process (current-buffer))))))

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

(defvar igs-player-re
  "\\([[:alpha:][:digit:]]+\\) +\\[ *\\([[:digit:]]+[kd]\\*\\)\\]"
  "Regular expression used to parse igs player name and rating.")

(defvar igs-game-re
  (format "\\[\\([[:digit:]]+\\)\\] +%s +vs. +%s +\\((.+)\\) \\((.+)\\)$"
          igs-player-re igs-player-re)
  "Regular expression used to parse igs game listings.")

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
