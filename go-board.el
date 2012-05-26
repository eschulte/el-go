;;; go-board.el --- Smart Game Format GO board visualization

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
(require 'go-util)
(require 'go-trans)

(defvar *history*  nil "Holds the board history for a GO buffer.")
(defvar *size*     nil "Holds the board size.")
(defvar *turn*     nil "Holds the color of the current turn.")
(defvar *back-end* nil "Holds the primary back-end connected to a board.")
(defvar *trackers* nil "Holds a list of back-ends which should track the game.")

(defvar black-piece "X")
(defvar white-piece "O")


;;; Board manipulation functions
(defun make-board (size) (make-vector (* size size) nil))

(defun board-size (board) (round (sqrt (length board))))

(defun move-type (move)
  (cond
   ((member (car move) '(:B  :W))  :move)
   ((member (car move) '(:LB :LW)) :label)))

(defun other-color (color)
  (if (equal color :B) :W :B))

(defun apply-turn-to-board (moves)
  (let ((board (pieces-to-board (car *history*) *size*)))
    (clear-labels board)
    (dolist (move moves) (apply-move board move))
    (push (board-to-pieces board) *history*)
    (update-display (current-buffer))))

(defun apply-move (board move)
  (flet ((bset (val data)
               (let ((data (if (listp (car data)) data (list data))))
                 (setf (aref board (pos-to-index (aget data :pos)
                                                 (board-size board)))
                       (case val
                         (:B  :B)
                         (:W  :W)
                         (:LB (aget data :label))
                         (:LW (aget data :label))
                         (t nil))))))
    (case (move-type move)
      (:move
       (bset (car move) (cdr move))
       (let ((color (if (equal :B (car move)) :B :W)))
         (remove-dead board (other-color color))
         (remove-dead board color)))
      (:label
       (dolist (data (cdr move)) (bset (car move) data))))))

(defun clear-labels (board)
  (dotimes (point (length board) board)
    (when (aref board point)
      (unless (member (aref board point) '(:B :W))
        (setf (aref board point) nil)))))

(defun neighbors (board piece)
  (let ((size (board-size board))
        neighbors)
    (when (not (= (mod piece size) (1- size))) (push (1+ piece) neighbors))
    (when (not (= (mod piece size) 0))         (push (1- piece) neighbors))
    (when (< (+ piece size) (length board))    (push (+ piece size) neighbors))
    (when (> (- piece size) 0)                 (push (- piece size) neighbors))
    neighbors))

(defun alive-p (board piece &optional already)
  (let* ((val (aref board piece))
         (enemy (other-color val))
         (neighbors (remove-if (lambda (n) (member n already))
                               (neighbors board piece)))
         (neighbor-vals (mapcar (lambda (n) (aref board n)) neighbors))
         (friendly (delete nil (mapcar
                                (lambda (n) (when (equal (aref board n) val) n))
                                neighbors)))
         (already (cons piece already)))
    (or (some (lambda (v) (not (or (equal v enemy) ; touching open space
                              (equal v val))))
              neighbor-vals)
        (some (lambda (n) (alive-p board n already)) ; touching alive dragon
              friendly))))

(defun remove-dead (board color)
  ;; must remove one color at a time for ko situations
  (let (cull)
    (dotimes (n (length board) board)
      (when (and (equal (aref board n) color) (not (alive-p board n)))
        (push n cull)))
    (dolist (n cull cull) (setf (aref board n) nil))))

(defun board-to-pieces (board)
  (let (pieces)
    (dotimes (n (length board) pieces)
      (let ((val (aref board n)))
        (when val (push (cons val n) pieces))))))

(defun pieces-to-board (pieces size)
  (let ((board (make-vector (* size size) nil)))
    (dolist (piece pieces board)
      (setf (aref board (cdr piece)) (car piece)))))


;;; Visualization
(defun board-header (board)
  (let ((size (board-size board)))
    (concat "    "
            (mapconcat (lambda (n)
                         (let ((char (+ ?A n)))
                           (when (>= char ?I)
                             (setq char (+ 1 char)))
                           (string char)))
                       (range size) " "))))

(defun board-pos-to-string (board pos)
  (let ((size (board-size board)))
    (flet ((emph (n)
                 (cond
                  ((= size 19)
                   (or (= 3 n)
                       (= 4 (- size n))
                       (= n (/ (- size 1) 2))))
                  ((= size 9)
                   (or (= 2 n)
                       (= 4 n))))))
      (let* ((val (aref board (pos-to-index pos size)))
             (str (cond
                   ((equal val :W) white-piece)
                   ((equal val :B) black-piece)
                   ((and (stringp val) (= 1 (length val)) val))
                   (t  (if (and (emph (car pos)) (emph (cdr pos))) "+" ".")))))
        (put-text-property 0 (length str) :pos (cons (cdr pos) (car pos)) str)
        str))))

(defun board-row-to-string (board row)
  (let* ((size (board-size board))
         (label (format "%3d" (1+ row)))
         (row-body ""))
    (dotimes (n size)
      (setq row-body
            (concat row-body
                    (board-pos-to-string board (cons row n))
                    " ")))
    (concat label " " (substring row-body 0 (1- (length row-body))) label)))

(defun board-body-to-string (board)
  (let ((board (transpose-array board)))
    (mapconcat (lambda (m) (board-row-to-string board m))
               (reverse (range (board-size board))) "\n")))

(defun board-to-string (board)
  (let ((header (board-header board))
        (body (board-body-to-string board)))
    (mapconcat #'identity (list header body header) "\n")))

(defun update-display (buffer)
  (with-current-buffer buffer
    (delete-region (point-min) (point-max))
    (goto-char (point-min))
    (insert "\n"
            (board-to-string
             (pieces-to-board (car *history*) *size*))
            "\n\n")
    (let ((comment (go<-comment *back-end*)))
      (when comment
        (insert (make-string (+ 6 (* 2 *size*)) ?=)
                "\n\n"
                comment)))
    (goto-char (point-min))))

(defun go-board-display (back-end &rest trackers)
  (let ((buffer (generate-new-buffer "*GO*")))
    (with-current-buffer buffer
      (go-board-mode)
      (let ((name (go<-name back-end)))
        (when name
          (rename-buffer (ear-muffs name) 'unique)
          (mapcar (lambda (tr) (go->name tr name)) trackers)))
      (set (make-local-variable '*back-end*) back-end)
      (set (make-local-variable '*turn*) :B)
      (set (make-local-variable '*size*) (go<-size back-end))
      (mapcar (lambda (tr) (go->size tr *size*)) trackers)
      (set (make-local-variable '*history*)
           (list (board-to-pieces (make-board *size*))))
      (set (make-local-variable '*trackers*) trackers)
      (update-display (current-buffer)))
    (pop-to-buffer buffer)))


;;; User input
(defmacro with-backends (sym &rest body)
  (declare (indent 1))
  `(prog1 (let ((,sym *back-end*)) ,@body)
     (mapcar (lambda (tr) (let ((,sym tr)) ,@body)) *trackers*)))

(defvar go-board-actions '(move resign undo comment)
  "List of actions which may be taken on an GO board.")

(defun go-board-act ()
  "Send a command to the current GO board."
  (interactive)
  (let ((command (org-icompleting-read
                  "Action: " (mapcar #'symbol-name go-board-actions))))
    (case (intern command)
      (move    (message "make a move"))
      (resign  (message "game over"))
      (undo    (message "loser"))
      (comment (message "what?")))))

(defun go-board-act-move (&optional pos)
  (interactive)
  (let* ((color (case *turn* (:B "black") (:W "white")))
         (pos (or pos (cons (go-gtp-char-to-num
                             (aref (downcase
                                    (org-icompleting-read
                                     (format "[%s] X pos: " color)
                                     (mapcar #'string
                                             (mapcar #'go-gtp-num-to-char
                                                     (range 1 *size*)))))
                                   0))
                            (1- (string-to-number
                                 (org-icompleting-read
                                  (format "[%s] Y pos: " color)
                                  (mapcar #'number-to-string
                                          (range 1 *size*))))))))
         (move (cons *turn* (cons :pos pos))))
    (with-backends back
      (go->move back move))
    (apply-turn-to-board (list move))
    (setf *turn* (other-color *turn*))))

(defun go-board-act-resign ()
  (interactive)
  (with-backends back (go->reset back)))

(defun go-board-act-undo (&optional num)
  (interactive "p")
  (with-backends back (go->undo back))
  (pop *history*)
  (update-display (current-buffer))
  (setf *turn* (other-color *turn*)))

(defun go-board-act-comment (&optional comment)
  (interactive "MComment: ")
  (message "comment: %S" comment))

(defun go-board-next (&optional count)
  (interactive "p")
  (dotimes (n (or count 1) (or count 1))
    (apply-turn-to-board (go<-turn *back-end* *turn*))
    (setf *turn* (other-color *turn*))))

(defun go-board-mouse-move (ev)
  (interactive "e")
  (go-board-act-move (get-text-property (posn-point (event-start ev)) :pos)))


;;; Display mode
(defvar go-board-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<down-mouse-1>") 'go-board-mouse-move)
    (define-key map (kbd "m") 'go-board-act-move)
    (define-key map (kbd "r") 'go-board-act-resign)
    (define-key map (kbd "u") 'go-board-act-undo)
    (define-key map (kbd "c") 'go-board-act-comment)
    (define-key map (kbd "n") 'go-board-next)
    (define-key map (kbd "p") 'go-board-act-undo)
    (define-key map (kbd "<right>") 'go-board-next)
    (define-key map (kbd "<left>")  'go-board-act-undo)
    (define-key map (kbd "q") (lambda () (interactive)
                                (kill-buffer (current-buffer))))
    map)
  "Keymap for `go-board-mode'.")

(define-derived-mode go-board-mode nil "GO"
  "Major mode for viewing a GO board.")

(defun go-board-play (&optional level)
  (interactive "P")
  (go-board-display
   (make-instance 'gnugo
     :buffer (apply #'go-gnugo-start-process
                    (when level (list "--level" (number-to-string level)))))))

(provide 'go-board)
