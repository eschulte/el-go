;;; sgf-play.el --- Play SGF-backed game of GO

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

;; Playing a game of go amounts to authoring an SGF file.  A game
;; requires two players and an SGF data store.  Those elements of the
;; game which are amenable to storage in SGF format will be added to
;; the file.

;; A player may be any of the following;
;; - a human inputting moves through an Emacs buffer
;; - an entity communicating through the Go Text Protocol (GTP)
;; - an entity communicating through the IGS protocol

;;; Code:
(require 'sgf2el)
(require 'sgf-board)
(eval-when-compile (require 'cl))

;; TODO: implement an API for back-end protocols

(provide 'sgf-play)
