;;; sgf.el --- Smart Game Format (focused on GO)

;; http://www.red-bean.com/sgf/sgf4.html
;; http://www.red-bean.com/sgf/properties.html

;;; BNF
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

;;; There are two types of property lists: 'list of' and 'elist of'. 
;; 'list of':    PropValue { PropValue }
;; 'elist of':   ((PropValue { PropValue }) | None)
;;               In other words elist is list or "[]".

;;; Property Value Types
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

;; an example is at the bottom of the page

;;; Comments:

;; - an sgf tree is just a series of nested lists.
;; - a pointer into the tree marks the current location
;; - navigation using normal Sexp movement
;; - games build such trees as they go
;; - a board is just one interface into such a tree

;;; Code:
(defun char-to-offset (char)
  (if (< char ?a)
      (+ 26 (- char ?A))
    (- char ?a)))

(defun parse-prop (str)
  (multiple-value-bind (id rest) (parse-prop-ident str)
    (cons id (parse-prop-vals rest))))

(defun parse-prop-ident (str)
  (let ((end (if (and (<= ?A (aref str 1))
                      (< (aref str 1) ?Z))
                 2 1)))
    (values (substring str 0 end)
            (substring str end))))

(defun parse-prop-vals (str)
  (let (res (start 0))
    (while (string-match "\\[\\(.*?[^\\]\\)\\]" str start)
      (push (match-string 1 str) res)
      (setq start (match-end 0)))
    res))


;;; Tests
(require 'ert)

(ert-deftest parse-prop-tests ()
  (flet ((should= (a b) (should (tree-equal a b :test #'string=))))
    (should= (parse-prop "B[pq]") '("B" "pq"))
    (should= (parse-prop "GM[1]") '("GM" "1"))
    (should (= (length (cdr (parse-prop "TB[as][bs][cq][cr][ds][ep]")))
               6))))

(defun parse-nodes (str)
  )
