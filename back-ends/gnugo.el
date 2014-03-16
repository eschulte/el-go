;;; gnugo.el --- gnugo GO back-end

;; Copyright (C) 2008 2012  Free Software Foundation, Inc.

;; Author: Eric Schulte <schulte.eric@gmail.com>
;; Created: 2012-05-15
;; Version: 0.1
;; Keywords: game go sgf gnugo

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

;;; CODE:
(require 'go-api)
(require 'gtp)
(require 'comint)

(defun gnugo-gtp-commands ()
  "Return a list of the gnugo GTP commands."
  (split-string
   (substring
    (shell-command-to-string
     (format "echo list_commands | %s --mode gtp" gnugo-program))
    2 -2) "\n"))

(defvar gnugo-program "gnugo"
  "Path to gnugo executable.")

(defvar gnugo-options nil
  "List of default options to gnugo.
For example, the following changes the level of gnugo.
  (setq gnugo-options (list \"--level\" \"2\"))")

(defvar gnugo-process-name "gnugo"
  "Name for the gnugo process.")

(defun gnugo-start-process (&rest options)
  (let ((buffer (apply 'make-comint
                       gnugo-process-name
                       gnugo-program nil
                       "--mode" "gtp" "--quiet"
                       (or options gnugo-options))))
    (with-current-buffer buffer (comint-mode))
    buffer))

(defun gnugo-command-to-string (gnugo command)
  "Send command to gnugo process and return gnugo's results as a string"
  (interactive "sgnugo command: ")
  (gnugo-input-command gnugo command)
  (gnugo-last-output gnugo))

(defun gnugo-input-command (gnugo command)
  "Pass COMMAND to the gnugo process running in the buffer of GNUGO."
  (with-current-buffer (buffer gnugo)
    (when (get-buffer-process (current-buffer))
      (goto-char (process-mark (get-buffer-process (current-buffer))))
      (insert command)
      (comint-send-input)))
  (gnugo-wait-for-output gnugo))

(defun gnugo-wait-for-output (gnugo)
  (condition-case e
      (with-current-buffer (buffer gnugo)
        (while (progn
                 (goto-char comint-last-input-end)
                 (not (re-search-forward "^= *[^\000]+?\n\n" nil t)))
          (when (re-search-forward "^? *\\([^\000]+?\\)\n\n" nil t)
            (error (match-string 1)))
          (accept-process-output (get-buffer-process (current-buffer)))))
    (error (message "gnugo error: %S" e))))

(defun gnugo-last-output (gnugo)
  (with-current-buffer (buffer gnugo)
    (comint-show-output)
    (go-clean-text-properties
     (buffer-substring (+ 2 (point)) (- (point-max) 2)))))


;;; Class and interface
(defclass gnugo (gtp)
  ((buffer :initarg :buffer :accessor buffer)))

(defmethod go-connect ((gnugo gnugo))
  (setf (buffer gnugo) (gnugo-start-process)))

(defmethod gtp-command ((gnugo gnugo) command)
  (gnugo-command-to-string gnugo command))

(defmethod go-player-name ((gnugo gnugo) color)
  "GNU GO")

(defmethod set-player-name ((gnugo gnugo) color name)
  (signal 'unsupported-back-end-command (list gnugo :set-player-name name)))

(defmethod go-dead ((gnugo gnugo))
  (mapcar (lambda (gtp-point) (gtp-to-pos nil gtp-point))
          (mapcar #'symbol-name
                  (read (format "(%s)"
                                (gnugo-command-to-string
                                 gnugo "final_status_list dead"))))))

(provide 'gnugo)
;;; gnugo.el ends here
