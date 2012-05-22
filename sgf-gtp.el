;;; sgf-gtp.el --- translate between sgf and GTP

;; Copyright (C) 2008 2012 Eric Schulte <eric.schulte@gmx.com>

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

;; Commentary:

;; This file should be useful for translating between sgf and the GO
;; text protocol (GTP) see http://www.lysator.liu.se/~gunnar/gtp/.
;; The GMP command set may be implemented as an extension.

;; Code:
(require 'cl)
(require 'sgf-util)

(defun sgf-gtp-char-to-pos (char)
  (flet ((err () (error "sgf-gtp: invalid char %s" char)))
    (cond
     ((< char ?A)  (err))
     ((< char ?I)  (1+ (- char ?A)))
     ((<= char ?T) (- char ?A))
     ((< char ?a)  (err))
     ((< char ?i)  (1+ (- char ?a)))
     ((<= char ?t) (- char ?a))
     (t (err)))))

(defun sgf-pos-to-gtp (pos)
  (format "%c%d" (num-to-char (car pos)) (1+ (cdr pos))))

(defun sgf-to-gtp-command (element)
  "Convert an sgf ELEMENT to a gtp command."
  (let ((key (car element))
	(val (cdr element)))
    (case key
      (:B       (format "black %s" (sgf-pos-to-gtp (aget (list val) :pos))))
      (:W       (format "white %s" (sgf-pos-to-gtp (aget (list val) :pos))))
      ((:SZ :S) (format "boardsize %s" val))
      (:KM      (format "komi %s" val))
      (t        nil))))

(provide 'sgf-gtp)
;;; sgf-gtp.el ends here
