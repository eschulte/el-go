;;; sgf-gnugo.el --- functions for interaction with a gnugo process using gtp

;; Copyright (C) 2008 2012 Eric Schulte <eric.schulte@gmx.com>

;;; Liscence:

;; Author: Eric Schulte <eric.schulte@gmx.com>
;; Created: 2012-05-15
;; Version: 0.1
;; Keywords: game go sgf gtp gnugo

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

;;; Comments:

;; Interaction with gnugo

;;; CODE:
(require 'comint)

(defun sgf-gnugo-gtp-commands ()
  "Return a list of the gnugo GTP commands."
  (split-string
   (substring
    (shell-command-to-string
     (format "echo list_commands | %s --mode gtp" sgf-gnugo-program))
    2 -2) "\n"))

(defvar sgf-gnugo-program "gnugo"
  "path to gnugo executable")

(defvar sgf-gnugo-process-name "gnugo"
  "name for the gnugo process")

(defvar sgf-gnugo-buffer nil
  "comint buffer holding the gnugo processes")

(defun sgf-gnugo-start-process (&optional options)
  (interactive)
  (unless (comint-check-proc sgf-gnugo-buffer)
    (setf sgf-gnugo-buffer
	  (apply 'make-comint
                 sgf-gnugo-process-name
                 sgf-gnugo-program nil
                 "--mode" "gtp" "--quiet"
		 (if options (split-string options))))
    (set-buffer sgf-gnugo-buffer)
    (comint-mode)
    ;; just to refresh everything
    (sgf-gnugo-input-command "showboard")))

(defun sgf-gnugo-command-to-string (command)
  "Send command to gnugo process and return gnugo's results as a string"
  (interactive "sgnugo command: ")
  (sgf-gnugo-input-command command)
  (sgf-gnugo-last-output))

(defun sgf-gnugo-input-command (command)
  "Pass COMMAND to the gnugo process running in `sgf-gnugo-buffer'"
  (save-excursion
    (message (format "buffer-%s" sgf-gnugo-buffer))
    (set-buffer sgf-gnugo-buffer)
    (goto-char (process-mark (get-buffer-process (current-buffer))))
    (insert command)
    (comint-send-input)
    (sgf-gnugo-wait-for-output)))

(defun sgf-gnugo-wait-for-output ()
  "Wait until output arrives"
  (save-excursion
    (set-buffer sgf-gnugo-buffer)
    (while (progn
	     (goto-char comint-last-input-end)
	     (not (re-search-forward "^= *[^\000]+?\n\n" nil t)))
      (if (re-search-forward "^? *\\([^\000]+?\\)\n\n" nil t)
	  (error (match-string 1)))
      (accept-process-output (get-buffer-process (current-buffer))))))

(defun sgf-gnugo-last-output ()
  (save-window-excursion
    (set-buffer sgf-gnugo-buffer)
    (comint-show-output)
    (buffer-substring (+ 2 (point)) (- (point-max) 2))))

(provide 'sgf-gnugo)
;;; sgf-gnugo.el ends here
