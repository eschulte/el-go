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
(require 'sgf2el)
(eval-when-compile (require 'cl))


;;; Visualization
(defvar sgf-board nil "Holds the board local to a GO buffer.")

(defvar sgf-sgf   nil "Holds the sgf data structure local to a GO buffer.")

(defvar sgf-index nil "Index into the sgf local to a GO buffer.")

(defun make-board (size) (make-vector (* size size) nil))

(defun board-size (board) (round (sqrt (length board))))

(defvar black-piece "X")

(defvar white-piece "O")

(defun board-header (board)
  (let ((size (board-size board)))
    (concat "    "
            (mapconcat (lambda (n)
                         (let ((char (+ ?A n)))
                           (when (>= char ?I)
                             (setq char (+ 1 char)))
                           (string char)))
                       (range size) " "))))

(defun pos-to-index (pos size)
  (+ (car pos) (* (cdr pos) size)))

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

(defun sgf-board-options ()
  (let ((count 0))
    (mapcar (lambda (alt)
              (prog1 (if (alistp alt)
                         count
                       (if (alistp (car alt))
                           (list count 0)
                         :other))
                (incf count)))
            (sgf-nthcdr *sgf* *index*))))

(defun get-create-pieces ()
  (let ((pieces (aget (sgf-ref sgf-sgf sgf-index) :pieces)))
    (if pieces
        (when (listp pieces) pieces)
      (clear-labels sgf-board)
      (apply-moves sgf-board (sgf-ref sgf-sgf sgf-index))
      (setq pieces (board-to-pieces sgf-board))
      (push (cons :pieces pieces) (sgf-ref sgf-sgf sgf-index))
      pieces)))

(defun update-display ()
  (unless sgf-sgf (error "sgf: buffer has not associated sgf data"))
  (delete-region (point-min) (point-max))
  (goto-char (point-min))
  (setq sgf-board (pieces-to-board (get-create-pieces) (length sgf-board)))
  (insert
   "\n"
   (board-to-string sgf-board)
   "\n\n")
  (let ((comment (aget (sgf-ref sgf-sgf sgf-index) :C)))
    (when comment
      (insert (make-string (+ 6 (* 2 (board-size sgf-board))) ?=)
              "\n\n")
      (insert comment)))
  (goto-char (point-min)))

(defun display-sgf (game)
  (let ((buffer (generate-new-buffer "*sgf*")))
    (with-current-buffer buffer
      (sgf-mode)
      (set (make-local-variable 'sgf-sgf)   game)
      (set (make-local-variable 'sgf-index) '(0))
      ;; TODO: this shouldn't be required
      (unless (tree-equal sgf-index '(0))
        (setq sgf-index '(0))
        (setf (car sgf-index) 0))
      (let* ((root (sgf-ref sgf-sgf sgf-index))
             (name (or (aget root :GN)
                       (aget root :EV)))
             (size (or (aget root :S) (aget root :SZ)
                       (unless (tree-equal sgf-index '(0))
                         (error "sgf: bad index %S" sgf-index))
                       (error "sgf: game has no associated size"))))
        (when name (rename-buffer name 'unique))
        (set (make-local-variable 'sgf-board) (make-board size))
        (update-display)))
    (pop-to-buffer buffer)))

(defun display-sgf-file (path)
  (interactive "f")
  (display-sgf (sgf2el-file-to-el path)))

(defun up (&optional num)
  (interactive "p")
  (prog1 (dotimes (n num n)
           (unless (alistp (sgf-ref sgf-sgf sgf-index))
             (update-display)
             (error "sgf: no more upwards moves."))
           (decf (car (last sgf-index 2)))
           (update-display))))

(defun down (&optional num)
  (interactive "p")
  (prog1 (dotimes (n num n)
           (incf (car (last sgf-index 2)))
           (setf (car (last sgf-index)) 0)
           (unless (alistp (sgf-ref sgf-sgf sgf-index))
             (update-display)
             (error "sgf: no more downwards moves."))
           (update-display))))

(defun left (&optional num)
  (interactive "p")
  (prog1 (dotimes (n num n)
           (unless (alistp (sgf-ref sgf-sgf sgf-index))
             (update-display)
             (error "sgf: no more backwards moves."))
           (decf (car (last sgf-index)))
           (update-display))))

(defun right (&optional num)
  (interactive "p")
  (prog1 (dotimes (n num n)
           (incf (car (last sgf-index)))
           (unless (alistp (sgf-ref sgf-sgf sgf-index))
             (decf (car (last sgf-index)))
             (update-display)
             (error "sgf: no more forward moves."))
           (update-display))))


;;; Board manipulation functions
(defun sgf-nthcdr (sgf index)
  (let ((part sgf))
    (while (cdr index)
      (setq part (nth (car index) part))
      (setq index (cdr index)))
    (setq part (nthcdr (car index) part))
    part))

(defun sgf-ref (sgf index)
  (let ((part sgf))
    (while (car index)
      (setq part (nth (car index) part))
      (setq index (cdr index)))
    part))

(defun set-sgf-ref (sgf index new)
  (eval `(setf ,(reduce (lambda (acc el) (list 'nth el acc))
                        index :initial-value 'sgf)
               ',new)))

(defsetf sgf-ref set-sgf-ref)

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
           (remove-dead sgf-board (other-color color))
           (remove-dead sgf-board color)))
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
