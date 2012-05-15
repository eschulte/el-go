;;; sgf.el --- Smart Game Format (focused on GO)

;; Copyright (C) 2012 Eric Schulte <eric.schulte@gmx.com>

;; Author: Eric Schulte <eric.schulte@gmx.com>
;; Created: 2012-05-15
;; Version: 0.1
;; Keywords: game go

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

;; This file implements a reader, writer and visualizer for sgf files.
;; The sgf format is defined at http://www.red-bean.com/sgf/sgf4.html.

;;; Syntax:

;; BNF
;; 
;; Collection = GameTree { GameTree }
;; GameTree   = "(" Sequence { GameTree } ")"
;; Sequence   = Node { Node }
;; Node       = ";" { Property }
;; Property   = PropIdent PropValue { PropValue }
;; PropIdent  = UcLetter { UcLetter }
;; PropValue  = "[" CValueType "]"
;; CValueType = (ValueType | Compose)
;; ValueType  = (None | Number | Real | Double | Color | SimpleText |
;;       	Text | Point  | Move | Stone)

;; Property Value Types
;; 
;; UcLetter   = "A".."Z"
;; Digit      = "0".."9"
;; None       = ""
;; Number     = [("+"|"-")] Digit { Digit }
;; Real       = Number ["." Digit { Digit }]
;; Double     = ("1" | "2")
;; Color      = ("B" | "W")
;; SimpleText = { any character (handling see below) }
;; Text       = { any character (handling see below) }
;; Point      = game-specific
;; Move       = game-specific
;; Stone      = game-specific
;; Compose    = ValueType ":" ValueType

;;; Comments:

;; - an sgf tree is just a series of nested lists.
;; - a pointer into the tree marks the current location
;; - navigation using normal Sexp movement
;; - games build such trees as they go
;; - a board is just one interface into such a tree

;;; Code:
(require 'cl)


;;; Parsing
(defmacro parse-many (regexp string &rest body)
  (declare (indent 2))
  `(let (res (start 0))
     (flet ((collect (it) (push it res)))
       (while (string-match ,regexp ,string start)
         (setq start (match-end 0))
         (save-match-data ,@body))
       (nreverse res))))
(def-edebug-spec parse-many (regexp string body))

(defvar parse-prop-val-re
  "[[:space:]\n\r]*\\[\\([^\000]*?[^\\]\\)\\]")

(defvar parse-prop-re
  (format "[[:space:]\n\r]*\\([[:alpha:]]+\\(%s\\)+\\)" parse-prop-val-re))

(defvar parse-node-re
  (format "[[:space:]\n\r]*;\\(\\(%s\\)+\\)" parse-prop-re))

(defvar parse-tree-part-re
  (format "[[:space:]\n\r]*(\\(%s\\)[[:space:]\n\r]*\\([()]\\)" parse-node-re))

(defun parse-prop-ident (str)
  (let ((end (if (and (<= ?A (aref str 1))
                      (< (aref str 1) ?Z))
                 2 1)))
    (values (substring str 0 end)
            (substring str end))))

(defun parse-prop-vals (str)
  (parse-many parse-prop-val-re str
    (collect (match-string 1 str))))

(defun parse-prop (str)
  (multiple-value-bind (id rest) (parse-prop-ident str)
    (cons id (parse-prop-vals rest))))

(defun parse-props (str)
  (parse-many parse-prop-re str
    (multiple-value-bind (id rest) (parse-prop-ident (match-string 1 str))
      (collect (cons id (parse-prop-vals rest))))))

(defun parse-nodes (str)
  (parse-many parse-node-re str
    (collect (parse-props (match-string 1 str)))))

(defun parse-trees (str)
  (let (cont-p)
    (parse-many parse-tree-part-re str
      (setq start (match-beginning 2))
      (let ((tree-part (parse-nodes (match-string 1 str))))
        (setq res (if cont-p
                      (list tree-part res)
                    (cons tree-part res)))
        (setq cont-p (string= (match-string 2 str) "("))))))

(defun read-from-buffer (buffer)
  (process (parse-trees (with-current-buffer buffer (buffer-string)))))

(defun read-from-file (file)
  (with-temp-buffer
    (insert-file-contents-literally file)
    (read-from-buffer (current-buffer))))


;;; Processing
(defvar sgf-property-alist nil
  "A-list of property names and the function to interpret their values.")

(defun process (raw)
  (unless (listp raw) (error "sgf: can't process atomic sgf element."))
  (if (listp (car raw))
      (mapcar #'process raw)
    (let ((func (cdr (assoc (car raw) sgf-property-alist))))
      (if func (cons (car raw) (funcall func (cdr raw))) raw))))

(defun process-date (date-args)
  (parse-time-string
   (if (> 1 (length date-args))
       (mapconcat #'number-to-string date-args " ")
     (car date-args))))
(add-to-list 'sgf-property-alist (cons "DT" #'process-date))

(defun process-board-size (size-args)
  (string-to-number (car size-args)))
(add-to-list 'sgf-property-alist (cons "S" #'process-board-size))

(defun char-to-pos (char)
  (cond
   ((or (< char ?A) (< ?z char))
    (error "sgf: invalid char %s" char))
   ((< char ?a) (+ 26 (- char ?A)))
   (t           (- (- char ?a) 1))))

(defun process-position (position-string)
  (cons (char-to-pos (aref position-string 0))
        (char-to-pos (aref position-string 1))))

(defun process-move (move-args)
  (process-position (car move-args)))
(add-to-list 'sgf-property-alist (cons "B" #'process-move))
(add-to-list 'sgf-property-alist (cons "W" #'process-move))

(defun process-label (label-args)
  (mapcar (lambda (l-arg)
            (message "l-arg:%s" l-arg)
            (if (string-match "\\([[:alpha:]]+\\):\\(.*\\)" l-arg)
                (cons (match-string 2 l-arg)
                      (process-position (match-string 1 l-arg)))
              (error "sgf: malformed label %S" l-arg)))
          label-args))
(add-to-list 'sgf-property-alist (cons "LB" #'process-label))
(add-to-list 'sgf-property-alist (cons "LW" #'process-label))


;;; Visualization
;; - make buffer to show a board, and notes, etc...
;; - keep an index into the sgf file
;; - write functions for building boards from sgf files (forwards and backwards)
;; - sgf movement keys

(defvar *board* nil "Holds the board local to a GO buffer.")
(make-variable-buffer-local '*board*)

(defvar *sgf*   nil "Holds the sgf data structure local to a GO buffer.")
(make-variable-buffer-local '*sgf*)

(defvar *index* nil "Index into the sgf data structure local to a GO buffer.")
(make-variable-buffer-local '*index*)

(defun make-board (size) (make-vector (* size size) nil))

(defun board-size (board) (round (sqrt (length board))))

(defun range (a &optional b)
  (block nil
    (let (tmp)
      (unless b
        (cond ((> a 0) (decf a))
              ((= a 0) (return nil))
              ((> 0 a) (incf a)))
        (setq b a a 0))
      (if (> a b) (setq tmp a a b b tmp))
      (let ((res (number-sequence a b)))
        (if tmp (nreverse res) res)))))

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
    (flet ((emph (n) (or (= 3 n)
                         (= 4 (- size n))
                         (= n (/ (- size 1) 2)))))
      (case (aref board (pos-to-index pos size))
        (:w white-piece)
        (:b black-piece)
        (t  (if (and (emph (car pos)) (emph (cdr pos))) "+" "."))))))

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

(defun update-display ()
  (delete-region (point-min) (point-max))
  (goto-char (point-min))
  (insert
   "\n"
   (board-to-string *board*)
   "\n\n")  
  (let ((comment (cdr (assoc "C" (sgf-ref *sgf* *index*)))))
    (when comment
      (insert (make-string (+ 6 (* 2 (board-size *board*))) ?=)
              "\n\n")
      (let ((beg (point)))
        (insert *comment*)
        (fill-region beg (point)))))
  (goto-char (point-min)))

(defun display-sgf (game)
  (let* ((root (car game))
         (name (format "*%s*"
                       (or (second (assoc "GN" root))
                           (second (assoc "EV" root))
                           "GO")))
         (buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (setf *sgf* game)
      (setf *board* (make-board (cdr (assoc "S" root))))
      (setf *index* '(1))
      (update-display))
    (pop-to-buffer buffer)))

(defun sgf-ref (sgf index)
  (let ((part sgf))
    (while (car index)
      (setq part (nth (car index) part))
      (setq index (cdr index)))
    part))

(defun up ())

(defun down ())

(defun left (&optional num)
  (interactive "p")
  (prog1 (dotimes (n num n)
           (decf (car (last *index*)))
           (unless (sgf-ref *sgf* *index*)
             (update-display)
             (error "sgf: no more backwards moves."))
           (revert-moves *board* (sgf-ref *sgf* *index*)))
    (update-display)))

(defun right (&optional num)
  (interactive "p")
  (prog1 (dotimes (n num n)
           (incf (car (last *index*)))
           (unless (sgf-ref *sgf* *index*)
             (update-display)
             (error "sgf: no more forward moves."))
           (apply-moves *board* (sgf-ref *sgf* *index*)))
    (update-display)))


;;; Board manipulation functions
(defun apply-moves (board moves)
  (dolist (move moves board)
    (setf (aref board (pos-to-index (cdr move) (board-size board)))
          (cond ((string= "B" (car move)) :b)
                ((string= "W" (car move)) :w)
                (t (error "sgf: invalid move %s" (car move)))))))

(defun revert-moves (board moves)
  (dolist (move moves board)
    (setf (aref board (pos-to-index (cdr move) (board-size board))) nil)))


;;; Tests
(require 'ert)

(ert-deftest sgf-parse-prop-tests ()
  (flet ((should= (a b) (should (tree-equal a b :test #'string=))))
    (should= (parse-props "B[pq]") '(("B" "pq")))
    (should= (parse-props "GM[1]") '(("GM" "1")))
    (should= (parse-props "GM[1]\nB[pq]\tB[pq]")
             '(("GM" "1") ("B" "pq") ("B" "pq")))
    (should (= (length (cdar (parse-props "TB[as][bs][cq][cr][ds][ep]")))
               6))))

(ert-deftest sgf-parse-multiple-small-nodes-test ()
  (let* ((str ";B[pq];W[dd];B[pc];W[eq];B[cp];W[cm];B[do];W[hq];B[qn];W[cj]")
         (nodes (parse-nodes str)))
    (should (= (length nodes) 10))
    (should (tree-equal (car nodes) '(("B" "pq")) :test #'string=))))

(ert-deftest sgf-parse-one-large-node-test ()
  (let* ((str ";GM[1]FF[4]
               SZ[19]
               GN[GNU Go 3.7.11 load and print]
               DT[2008-12-14]
               KM[0.0]HA[0]RU[Japanese]AP[GNU Go:3.7.11]AW[ja][oa]
               [pa][db][eb]")
         (node (car (parse-nodes str))))
    (should (= (length node) 10))
    (should (= (length (cdar (last node))) 5))))

(ert-deftest sgf-parse-simple-tree ()
  (let* ((str "(;GM[1]FF[4]
               SZ[19]
               GN[GNU Go 3.7.11 load and print]
               DT[2008-12-14]
               KM[0.0]HA[0]RU[Japanese]AP[GNU Go:3.7.11]AW[ja][oa]
               [pa][db][eb])")
         (tree (parse-trees str)))
    (should (= 1  (length tree)))
    (should (= 1  (length (car tree))))
    (should (= 10 (length (caar tree))))))

(ert-deftest sgf-parse-nested-tree ()
  (let* ((str "(;GM[1]FF[4]
               SZ[19]
               GN[GNU Go 3.7.11 load and print]
               DT[2008-12-14]
               KM[0.0]HA[0]RU[Japanese]AP[GNU Go:3.7.11]
               (;AW[ja][oa][pa][db][eb] ;AB[fa][ha][ia][qa][cb]))")
         (tree (parse-trees str)))
    (should (= 2  (length tree)))
    (should (= 9 (length (car (first tree)))))
    (should (= 2 (length (second tree))))))

(ert-deftest sgf-parse-file-test ()
  (let ((game (car (read-from-file "sgf-files/jp-ming-5.sgf"))))
    (should (= 247 (length game)))))

(ert-deftest sgf-empty-board-to-string-test ()
  (let ((board (make-vector (* 19 19) nil))
        (string (concat "    A B C D E F G H J K L M N O P Q R S T\n"
                        " 19 . . . . . . . . . . . . . . . . . . . 19\n"
                        " 18 . . . . . . . . . . . . . . . . . . . 18\n"
                        " 17 . . . . . . . . . . . . . . . . . . . 17\n"
                        " 16 . . . + . . . . . + . . . . . + . . . 16\n"
                        " 15 . . . . . . . . . . . . . . . . . . . 15\n"
                        " 14 . . . . . . . . . . . . . . . . . . . 14\n"
                        " 13 . . . . . . . . . . . . . . . . . . . 13\n"
                        " 12 . . . . . . . . . . . . . . . . . . . 12\n"
                        " 11 . . . . . . . . . . . . . . . . . . . 11\n"
                        " 10 . . . + . . . . . + . . . . . + . . . 10\n"
                        "  9 . . . . . . . . . . . . . . . . . . .  9\n"
                        "  8 . . . . . . . . . . . . . . . . . . .  8\n"
                        "  7 . . . . . . . . . . . . . . . . . . .  7\n"
                        "  6 . . . . . . . . . . . . . . . . . . .  6\n"
                        "  5 . . . . . . . . . . . . . . . . . . .  5\n"
                        "  4 . . . + . . . . . + . . . . . + . . .  4\n"
                        "  3 . . . . . . . . . . . . . . . . . . .  3\n"
                        "  2 . . . . . . . . . . . . . . . . . . .  2\n"
                        "  1 . . . . . . . . . . . . . . . . . . .  1\n"
                        "    A B C D E F G H J K L M N O P Q R S T")))
    (should (string= string (board-to-string board)))))

(ert-deftest sgf-non-empty-board-to-string-test ()
  (let* ((joseki (car (read-from-file "sgf-files/3-4-joseki.sgf")))
         (root (car joseki))
         (rest (cdr joseki))
         (board (make-board (cdr (assoc "S" root))))
         (string (concat "    A B C D E F G H J K L M N O P Q R S T\n"
                         " 19 . . . . . . . . . . . . . . . . . . . 19\n"
                         " 18 . . . . . . . . . . . . . . . . . . . 18\n"
                         " 17 . . . . . . . . . . . . . . . . . . . 17\n"
                         " 16 . . . + . . . . . + . . . . . + . . . 16\n"
                         " 15 . . . . . . . . . . . . . . . . . . . 15\n"
                         " 14 . . . . . . . . . . . . . . . . . . . 14\n"
                         " 13 . . . . . . . . . . . . . . . . . . . 13\n"
                         " 12 . . . . . . . . . . . . . . . . . . . 12\n"
                         " 11 . . . . . . . . . . . . . . . . . . . 11\n"
                         " 10 . . X + . . . . . + . . . . . + . . . 10\n"
                         "  9 . . . . . . . . . . . . . . . . . . .  9\n"
                         "  8 . . . . . . . . . . . . . . . . . . .  8\n"
                         "  7 . . . . . . . . . . . . . . . . . . .  7\n"
                         "  6 . . . . . . . . . . . . . . . . . . .  6\n"
                         "  5 . . . X . . . . . . . . . . . . . . .  5\n"
                         "  4 . . X + O . O . . + . . . . . + . . .  4\n"
                         "  3 . . . X O . . . . . O . . . . . . . .  3\n"
                         "  2 . . . X . . . . . . . . . . . . . . .  2\n"
                         "  1 . . . . . . . . . . . . . . . . . . .  1\n"
                         "    A B C D E F G H J K L M N O P Q R S T")))
    (dolist (moves rest)
      (apply-moves board moves))
    (board-to-string board)
    (should t)))

(ert-deftest sgf-display-fresh-sgf-buffer ()
  (let* ((joseki (car (read-from-file "sgf-files/3-4-joseki.sgf")))
         (buffer (display-sgf joseki)))
    (with-current-buffer buffer
      (should *board*)
      (should *sgf*)
      (should *index*))
    (should (kill-buffer buffer))))
