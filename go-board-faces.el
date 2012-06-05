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


;;; Images
(defvar go-board-image-overlays nil
  "List of overlays carrying the images of points on a GO board.")

(defun go-board-svg-trans (list)
  (if (and (listp list) (listp (car list)))
      (concat (format "<%s%s" (caar list) (if (cdar list) " " ""))
              (mapconcat (lambda (pair) (format "%s=\"%s\"" (car pair) (cdr pair)))
                         (cdar list) " ")
              (if (cdr list)
                  (concat ">"
                          (mapconcat #'go-board-svg-trans (cdr list) " ")
                          (format "</%s>" (caar list)))
                "/>"))
    list))

(defmacro go-board-wrap (&rest body)
  `(concat
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    (go-board-svg-trans
     '((svg (xmlns . "http://www.w3.org/2000/svg")
            (xmlns:xlink . "http://www.w3.org/1999/xlink")
            (width . 25) (height . 25) (version . 1.0))
       ,@body))))

(defmacro go-board-image (&rest body)
  ``(image :type svg :ascent center :data
           ,(go-board-wrap
              ((rect (width . 25) (height . 25) (fill . "#dcb35c")))
              ,@body)))

(defmacro go-board-side (path)
  `(go-board-image ((path (stroke . "#000") (stroke-width . 1) (d . ,path)))))

(defun go-board-add-image (point image)
  (let ((ov (make-overlay point (1+ point))))
    (overlay-put ov 'display image)
    (push ov go-board-image-overlays)))

(defvar go-board-image-black
  (go-board-image
   ((defs)
    ((radialGradient (id . "$rg") (cx . ".3") (cy . ".3") (r . ".8"))
     ((stop (offset . 0)   (stop-color . "#777")))
     ((stop (offset . 0.3) (stop-color . "#222")))
     ((stop (offset . 1)   (stop-color . "#000")))))
   ((circle (cx . 12.5) (cy . 12.5) (r . 6.125) (fill . "url(#$rg)")))))

(defvar go-board-image-white
  (go-board-image
   ((defs)
    ((radialGradient (id . "$rg") (cx . ".47") (cy . ".49") (r . ".48"))
     ((stop (offset . 0.7) (stop-color . "#FFF")))
     ((stop (offset . 0.9) (stop-color . "#DDD")))
     ((stop (offset . 1)   (stop-color . "#777")))))
   ((circle (cx . 12.5) (cy . 12.5) (r . 6.125) (fill . "url(#$rg)")))))

(defvar go-board-image-back
  (go-board-image
   ((path (stroke . "#000") (stroke-width . 1) (d . "M0,12.5H25M12.5,0V25")))))

(defvar go-board-image-hoshi
  (go-board-image
   ((path (stroke . "#000") (stroke-width . 1) (d . "M0,12.5H25M12.5,0V25")))
   ((circle (cx . 12.5) (cy . 12.5) (r . 2.5)))))

(defvar go-board-image-left         (go-board-side "M12,12.5H25M12.5,0V25"))
(defvar go-board-image-right        (go-board-side "M0,12.5H13M12.5,0V25"))
(defvar go-board-image-top          (go-board-side "M0,12.5H25M12.5,12V25"))
(defvar go-board-image-bottom       (go-board-side "M0,12.5H25M12.5,0V12.5"))
(defvar go-board-image-top-left     (go-board-side "M12,12.5H25M12.5,12V25"))
(defvar go-board-image-top-right    (go-board-side "M0,12.5H13M12.5,12V25"))
(defvar go-board-image-bottom-left  (go-board-side "M12,12.5H25M12.5,0V13"))
(defvar go-board-image-bottom-right (go-board-side "M0,12.5H13M12.5,0V13"))

(defmacro go-board-image-label (label)
  `(go-board-image
    ((text (x . 8.75) (y . 16.25) (r . 12.25) (style . "font-size:12.5;"))
     ,label)))

(defmacro go-board-image-white-label (label)
  `(go-board-image
    ((defs)
     ((radialGradient (id . "$rg") (cx . ".47") (cy . ".49") (r . ".48"))
      ((stop (offset . 0.7) (stop-color . "#FFF")))
      ((stop (offset . 0.9) (stop-color . "#DDD")))
      ((stop (offset . 1)   (stop-color . "#777")))))
    ((circle (cx . 12.5) (cy . 12.5) (r . 6.125) (fill . "url(#$rg)")))
    ((text (x . 8.75) (y . 16.25) (r . 12.25) (style . "font-size:12.5;"))
     ,label)))

(defmacro go-board-image-black-label (label)
  `(go-board-image
    ((defs)
     ((radialGradient (id . "$rg") (cx . ".3") (cy . ".3") (r . ".8"))
      ((stop (offset . 0)   (stop-color . "#777")))
      ((stop (offset . 0.3) (stop-color . "#222")))
      ((stop (offset . 1)   (stop-color . "#000")))))
    ((circle (cx . 12.5) (cy . 12.5) (r . 6.125) (fill . "url(#$rg)")))
    ((text (x . 8.75) (y . 16.25) (r . 12.25)
           (style . "font-size:12.5;fill:#ffffff;")) ,label)))

(provide 'go-board-faces)
