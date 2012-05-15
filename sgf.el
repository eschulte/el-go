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

(defun parse-from-buffer (buffer)
  (parse-trees (with-current-buffer buffer (buffer-string))))

(defun parse-from-file (file)
  (with-temp-buffer
    (insert-file-contents-literally file)
    (parse-from-buffer (current-buffer))))


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

(defun process-position (position-string)
  (flet ((char-to-pos (char)
           (cond
            ((or (< char ?A) (> char ?z)) (error "sgf: invalid char %s" char))
            ((< char ?a) (+ 26 (- char ?A)))
            (t (- char ?a)))))
    (cons (char-to-pos (aref position-string 0))
          (char-to-pos (aref position-string 1)))))

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

;; (defvar *board* (make-vector (* 19 19) nil))
(defun board-to-string (board)
  (let ((size (sqrt (length board))))
    (flet ((header ()
             (concat 
              "    "
              (let ((row ""))
                (dotimes (n size row)
                  (setq row (concat row (string (+ ?A n)) " "))))
              "\n"))
           (emph (n) (or (= 3 n)
                         (= 4 (- size n))
                         (= n (/ (- size 1) 2))))
           (body ()
             (let ((body ""))
               (dotimes (m size body)
                 (setq body
                       (concat body
                               (format "%3d" (- size m))
                               (let ((row " "))
                                 (dotimes (n size row)
                                   (setq row (concat row
                                                     (if (and (emph n) (emph m))
                                                         "+ "
                                                       ". ")))))
                               (format "%2d" (- size m))
                               "\n"))))))
      (concat (header) (body) (header)))))


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
  (let ((game (car (parse-from-file "games/jp-ming-5.sgf"))))
    (should (= 247 (length game)))))
