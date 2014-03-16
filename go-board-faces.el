;;; go-board-faces.el -- Color for GO boards

;; Copyright (C) 2012  Free Software Foundation, Inc.

;; Author: Eric Schulte <schulte.eric@gmail.com>
;; Created: 2012-05-15
;; Version: 0.1
;; Keywords: game go sgf

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

;; Maybe use `face-remap-add-relative' to change image sizes.


;;; Image utility functions
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

(defun go-board-cross (color)
  (mapconcat #'go-board-svg-trans
             `(((line (x1 . 3.125) (y1 . 3.125) (x2 . 21.875) (y2 . 21.875)
                      (style . ,(format "stroke: %s;" color))))
               ((line (x1 . 3.125) (y1 . 21.875) (x2 . 21.875) (y2 . 3.125)
                      (style . ,(format "stroke: %s;" color)))))
             ""))

(defun go-board-label (color label)
  (go-board-svg-trans
   `((text (x . 8.75) (y . 16.25) (r . 12.25)
           (style . ,(format "font-size:12.5;fill:%s;" color)))
     ,label)))

(defun go-board-mark (overlay mark)
  "Write MARK over top of the SVG image in OVERLAY."
  (let* ((disp (cdr (copy-tree (overlay-get overlay 'display))))
         (data (plist-get disp :data)))
    (when (and data (string-match (regexp-quote "</svg>") data))
      (plist-put disp :data (concat (substring data 0 (match-beginning 0))
                                    mark
                                    (substring data (match-beginning 0))))
      (overlay-put overlay 'display (cons 'image disp)))))

(defmacro go-board-wrap (&rest body)
  `(concat
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    (go-board-svg-trans
     '((svg (xmlns . "http://www.w3.org/2000/svg")
            (xmlns:xlink . "http://www.w3.org/1999/xlink")
            (width . 25) (height . 25) (version . 1.0))
       ,@body))))

;; TODO: To allow images to scale with text, this should return a
;;       function instead of a list.  This function should take a base
;;       size (e.g., 12.5), and should return the image list
;;       appropriate for that size.
(defmacro go-board-image (&rest body)
  ``(image :type svg :ascent center :data
           ,(go-board-wrap
              ((rect (width . 25) (height . 25) (fill . "#dcb35c")))
              ,@body)))

(defmacro go-board-image-sides (name &rest base)
  (declare (indent 1))
  `(progn
     ,@(mapcar
        (lambda (p)
          `(defvar ,(sym-cat 'go-board-image name (car p))
             (go-board-image
              ,(when (cdr p)
                 `((path (stroke . "#000") (stroke-width . 1) (d . ,(cdr p)))))
              ,@base)))
        '((left         . "M12,12.5H25M12.5,0V25")
          (right        . "M0,12.5H13M12.5,0V25")
          (top          . "M0,12.5H25M12.5,12V25")
          (bottom       . "M0,12.5H25M12.5,0V12.5")
          (top-left     . "M12,12.5H25M12.5,12V25")
          (top-right    . "M0,12.5H13M12.5,12V25")
          (bottom-left  . "M12,12.5H25M12.5,0V13")
          (bottom-right . "M0,12.5H13M12.5,0V13")
          (nil          . "M0,12.5H25M12.5,0V25")))))


;;; SVG Images
(go-board-image-sides background)

(go-board-image-sides black
  ((defs)
   ((radialGradient (id . "$rg") (cx . ".3") (cy . ".3") (r . ".8"))
    ((stop (offset . 0)   (stop-color . "#777")))
    ((stop (offset . 0.3) (stop-color . "#222")))
    ((stop (offset . 1)   (stop-color . "#000")))))
  ((circle (cx . 12.5) (cy . 12.5) (r . 9.375) (fill . "url(#$rg)"))))

(go-board-image-sides white
  ((defs)
   ((radialGradient (id . "$rg") (cx . ".47") (cy . ".49") (r . ".48"))
    ((stop (offset . 0.7) (stop-color . "#FFF")))
    ((stop (offset . 0.9) (stop-color . "#DDD")))
    ((stop (offset . 1)   (stop-color . "#777")))))
  ((circle (cx . 12.5) (cy . 12.5) (r . 9.375) (fill . "url(#$rg)"))))

(defvar go-board-image-hoshi
  (go-board-image
   ((path (stroke . "#000") (stroke-width . 1) (d . "M0,12.5H25M12.5,0V25")))
   ((circle (cx . 12.5) (cy . 12.5) (r . 2.5)))))

(provide 'go-board-faces)
;;; go-board-faces.el ends here
