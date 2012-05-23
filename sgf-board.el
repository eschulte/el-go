;;; sgf-board.el --- Smart Game Format GO board visualization

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
(require 'sgf-util)
(require 'sgf2el)

(defvar *board* nil "Holds the board local to a GO buffer.")

(defvar *backends* nil "Holds the back-ends connected to a board.")

(defvar black-piece "X")

(defvar white-piece "O")


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
      (let ((val (aref board (pos-to-index pos size))))
        (cond
         ((equal val :W) white-piece)
         ((equal val :B) black-piece)
         ((and (stringp val) (= 1 (length val)) val))
         (t  (if (and (emph (car pos)) (emph (cdr pos))) "+" ".")))))))

(defun board-row-to-string (board row)
  (let* ((size (board-size board))
         (label (format "%3d" (- size row)))
         (row-body (mapconcat
                    (lambda (n)
                      (board-pos-to-string board (cons row n)))
                    (range size) " ")))
    (concat label " " row-body label)))

(defun board-body-to-string (board)
  (mapconcat (lambda (m) (board-row-to-string board m))
             (range (board-size board)) "\n"))

(defun board-to-string (board)
  (let ((header (board-header board))
        (body (board-body-to-string board)))
    (mapconcat #'identity (list header body header) "\n")))

(defun board-to-pieces (board)
  (let (pieces)
    (dotimes (n (length board) pieces)
      (let ((val (aref board n)))
        (when val (push (cons val n) pieces))))))

(defun pieces-to-board (pieces size)
  (let ((board (make-vector size nil)))
    (dolist (piece pieces board)
      (setf (aref board (cdr piece)) (car piece)))))

(defun get-create-pieces ()
  (let ((pieces (aget (sgf-ref *sgf* *index*) :pieces)))
    (if pieces
        (when (listp pieces) pieces)
      (clear-labels *board*)
      (apply-moves *board* (sgf-ref *sgf* *index*))
      (setq pieces (board-to-pieces *board*))
      (push (cons :pieces pieces) (sgf-ref *sgf* *index*))
      pieces)))

(defun update-display ()
  (unless *sgf* (error "sgf: buffer has not associated sgf data"))
  (delete-region (point-min) (point-max))
  (goto-char (point-min))
  (setq *board* (pieces-to-board (get-create-pieces) (length *board*)))
  (insert
   "\n"
   (board-to-string *board*)
   "\n\n")
  (let ((comment (aget (sgf-ref *sgf* *index*) :C)))
    (when comment
      (insert (make-string (+ 6 (* 2 (board-size *board*))) ?=)
              "\n\n")
      (insert comment)))
  (goto-char (point-min)))

(defun display (game)
  (let ((buffer (generate-new-buffer "*sgf*")))
    (with-current-buffer buffer
      (sgf-mode)
      (set (make-local-variable '*sgf*)   game)
      (set (make-local-variable '*index*) '(0))
      ;; TODO: this shouldn't be required
      (unless (tree-equal *index* '(0))
        (setq *index* '(0))
        (setf (car *index*) 0))
      (let* ((root (sgf-ref *sgf* *index*))
             (name (or (aget root :GN)
                       (aget root :EV)))
             (size (or (aget root :S) (aget root :SZ)
                       (unless (tree-equal *index* '(0))
                         (error "sgf: bad index %S" *index*))
                       (error "sgf: game has no associated size"))))
        (when name (rename-buffer name 'unique))
        (set (make-local-variable '*board*) (make-board size))
        (update-display)))
    (pop-to-buffer buffer)))


;;; Board manipulation functions
(defun make-board (size) (make-vector (* size size) nil))

(defun board-size (board) (round (sqrt (length board))))

(defun pos-to-index (pos size)
  (+ (car pos) (* (cdr pos) size)))

(defun move-type (move)
  (cond
   ((member (car move) '(:B  :W))  :move)
   ((member (car move) '(:LB :LW)) :label)))

(defun other-color (color)
  (if (equal color :B) :W :B))

(defun apply-moves (board moves)
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
    (dolist (move moves board)
      (case (move-type move)
        (:move
         (bset (car move) (cdr move))
         (let ((color (if (equal :B (car move)) :B :W)))
           (remove-dead *board* (other-color color))
           (remove-dead *board* color)))
        (:label
         (dolist (data (cdr move)) (bset (car move) data)))))))

(defun clear-labels (board)
  (dotimes (point (length board))
    (when (aref board point)
      (unless (member (aref board point) '(:B :W))
        (setf (aref board point) nil)))))

(defun stones-for (board color)
  (let ((count 0))
    (dotimes (n (length board) count)
      (when (equal color (aref board n)) (incf count)))))

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
         (friendly-neighbors (delete nil (map 'list (lambda (n v)
                                                      (when (equal v val) n))
                                              neighbors neighbor-vals)))
         (already (cons piece already)))
    (or (some (lambda (v) (not (or (equal v enemy) ; touching open space
                              (equal v val))))
              neighbor-vals)
        (some (lambda (n) (alive-p board n already)) ; touching alive dragon
              friendly-neighbors))))

(defun remove-dead (board color)
  ;; must remove one color at a time for ko situations
  (let (cull)
    (dotimes (n (length board) board)
      (when (and (equal (aref board n) color) (not (alive-p board n)))
        (push n cull)))
    (dolist (n cull cull) (setf (aref board n) nil))))


;;; User input
(defvar sgf-board-actions '(move resign undo comment)
  "List of actions which may be taken on an SGF board.")

(defun sgf-board-act ()
  "Send a command to the current SGF board."
  (interactive)
  (let ((command (org-icompleting-read
                  "Action: " (mapcar #'symbol-name sgf-board-actions))))
    (case (intern command)
      (move    (message "make a move"))
      (resign  (message "game over"))
      (undo    (message "loser"))
      (comment (message "what?")))))

(defun sgf-board-act-move (&optional pos)
  (interactive)
  (unless pos
    (let ((size (if *board* (board-size *board*) 19)))
      (setq pos
            (cons
             (char-to-num
              (aref (downcase
                     (org-icompleting-read
                      "X pos: "
                      (mapcar #'string
                              (mapcar #'num-to-char (range 1 size)))))
                    0))
             (1- (string-to-number
                  (org-icompleting-read
                   "Y pos: "
                   (mapcar #'number-to-string (range 1 size)))))))))
  (message "move: %S" pos))

(defun sgf-board-act-resign ()
  (interactive)
  (message "resign"))

(defun sgf-board-act-undo (&optional num)
  (interactive "p")
  (message "undo: %S" num))

(defun sgf-board-act-comment (&optional comment)
  (interactive "MComment: ")
  (message "comment: %S" comment))


;;; Display mode
(defvar sgf-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<right>") 'right)
    (define-key map (kbd "<left>")  'left)
    (define-key map (kbd "<up>")    'up)
    (define-key map (kbd "<down>")  'down)
    (define-key map (kbd "q") (lambda () (interactive)
                                (kill-buffer (current-buffer))))
    map)
  "Keymap for `sgf-mode'.")

(define-derived-mode sgf-mode nil "SGF"
  "Major mode for editing text written for viewing SGF files.")

(provide 'sgf-board)
