;;; go-board-faces.el -- Color for GO boards

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

;;; Code:
(defface go-board-background
  '((t (:background "#b36108" :foreground "#6f3c04")))
  "woodsy background")

(defface go-board-hoshi
  '((t (:background "#b36108" :foreground "#6d3300")))
  "woodsy background with darker hoshi mark")

(defface go-board-black
  '((t (:background "#b36108" :foreground "black")))
  "black piece on woodsy background")

(defface go-board-white
  '((t (:background "#b36108" :foreground "white")))
  "white piece on woodsy background")

(defface go-board-black-territory-background
  '((t (:background "#6a4014" :foreground "#6f3c04")))
  "woodsy background")

(defface go-board-black-territory-hoshi
  '((t (:background "#6a4014" :foreground "#6d3300")))
  "woodsy background with darker hoshi mark")

(defface go-board-black-territory-black
  '((t (:background "#6a4014" :foreground "black")))
  "black piece on black territory")

(defface go-board-black-territory-white
  '((t (:background "#6a4014" :foreground "#6b6b6b")))
  "white piece on black territory")

(defface go-board-white-territory-background
  '((t (:background "#cd9c67" :foreground "#6f3c04")))
  "white territory")

(defface go-board-white-territory-hoshi
  '((t (:background "#cd9c67" :foreground "#6d3300")))
  "white territory with darker hoshi mark")

(defface go-board-white-territory-black
  '((t (:background "#cd9c67" :foreground "#6b6b6b")))
  "black piece on white territory")

(defface go-board-white-territory-white
  '((t (:background "#cd9c67" :foreground "white")))
  "white piece on white territory")

(provide 'go-board-faces)
