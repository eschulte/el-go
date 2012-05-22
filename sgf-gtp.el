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
(defun sgf-gtp-char-to-gtp (char)
  (flet ((err () (error "sgf-gtp: invalid char %s" char)))
    (cond
     ((< char ?A)  (err))
     ((< char ?I)  (1+ (- char ?A)))
     ((<= char ?T) (- char ?A))
     ((< char ?a)  (err))
     ((< char ?i)  (1+ (- char ?a)))
     ((<= char ?t) (- char ?a))
     (t (err)))))

(defun sgf-gtp-point-to-gtp (point-string)
  (format "%s%d"
	  (substring point-string 0 1)
	  (sgf-gtp-char-to-gtp (elt point-string 1))))

(defun sgf-gtp-command-to-sgf (command)
  "Convert a gtp command to an sgf element"
  (interactive)
  (unless (listp node)
    (error "sgf-gtp: node is not a cons cell"))
  (let ((symbol (car node))
	(value (cdr node)))
    (if (listp symbol) ; recurse
	(flatten (delq nil (mapcar 'sgf-gtp-node-to-gtp node)))
      (if (symbolp symbol)
	  (list
	   (case symbol
	    (':B
	     (format "black %s" (sgf-gtp-point-to-gtp-point value)))
	    (':W
	     (format "white %s" (sgf-gtp-point-to-gtp-point value)))
	    (':SZ
	     (format "boardsize %s" value))
	    (':KM
	     (format "komi %s" value))
	    (t
	     nil)))
	(error "sgf-gtp: %S is not a symbol" symbol)))))

(provide 'sgf-gtp)
;;; sgf-gtp.el ends here
