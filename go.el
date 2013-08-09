;;; go.el --- Play GO, translate and transfer between GO back ends

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

;;; Commentary:

;; A board-based interface to GO games which may be connected to a
;; number of GO back-ends through a generic API.  To play a game of GO
;; against the gnugo back-end run `play-go'.  Current back-ends
;; include the following.
;; - the SGF format
;; - the Go Text Protocol (GTP)
;; - TODO: the IGS protocol

;;; Code:
(let ((load-path
       (cons (file-name-directory (or load-file-name (buffer-file-name)))
             load-path)))
  (require 'go-util         "go-util.el")
  (require 'go-api          "go-api.el")
  (require 'go-board        "go-board.el")
  (require 'go-board-faces  "go-board-faces.el")
  (require 'gtp             "back-ends/gtp.el")
  (require 'gnugo           "back-ends/gnugo.el")
  (require 'sgf             "back-ends/sgf.el")
  (require 'sgf2el          "back-ends/sgf2el.el")
  (require 'igs             "back-ends/igs.el")
  (require 'gtp-pipe        "back-ends/gtp-pipe.el"))

(defun go-instantiate (back-end)
  (interactive)
  ;; TODO: read and set handicap.
  (let ((it (make-instance back-end))
        (size (read (org-icompleting-read
                     "board size: "
                     (mapcar #'number-to-string '(19 13 9))))))
    (go-connect it)
    (setf (go-size it) size)
    it))

;;;###autoload
(defun go-play ()
  "Play a game of GO."
  (interactive)
  (let ((back-end (case (intern (org-icompleting-read
                                 "play against: " '("gnugo" "person")))
                    (gnugo  (go-instantiate 'gnugo))
                    (person (go-instantiate 'sgf)))))
    (with-current-buffer (apply #'go-board
                                (cons back-end
                                      (unless (equal (class-of back-end) 'sgf)
                                        (list (make-instance 'sgf)))))
      (unless (equal (class-of back-end) 'sgf)
        (setq *autoplay* t)))))

;;;###autoload
(defun go-view-sgf (&optional file)
  "View an SGF file."
  (interactive "fSGF file: ")
  (let* ((sgf (make-instance 'sgf :self (sgf2el-file-to-el file) :index '(0)))
         (buffer (go-board sgf)))
    (with-current-buffer buffer
      (setf (index *back-end*) (list 0)))))

(provide 'go)
