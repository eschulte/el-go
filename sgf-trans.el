;;; sgf-trans.el --- Translate and transfer between GO back ends

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

;; An API for transferring GO moves and data between a number of GO
;; back ends including the following.
;; - the SGF format
;; - the Go Text Protocol (GTP)
;; - the IGS protocol

;;; Code:
(require 'sgf-util)
(require 'eieio)

(defgeneric sgf->move    (back-end move)    "Send MOVE to BACK-END.")
(defgeneric sgf->board   (back-end size)    "Send SIZE to BACK-END.")
(defgeneric sgf->resign  (back-end resign)  "Send RESIGN to BACK-END.")
(defgeneric sgf->undo    (back-end undo)    "Send UNDO to BACK-END.")
(defgeneric sgf->comment (back-end comment) "Send COMMENT to BACK-END.")
(defgeneric sgf<-move    (back-end)         "Get POS from BACK-END.")
(defgeneric sgf<-board   (back-end)         "Get SIZE from BACK-END.")
(defgeneric sgf<-comment (back-end)         "Get COMMENT from BACK-END.")

(provide 'sgf-trans)
