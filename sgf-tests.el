;;; sgf2el.el --- conversion between sgf and emacs-lisp

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
(require 'sgf-board)
(require 'ert)

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
    (should (= 10 (length (first tree))))))

(ert-deftest sgf-parse-nested-tree ()
  (let* ((str "(;GM[1]FF[4]
               SZ[19]
               GN[GNU Go 3.7.11 load and print]
               DT[2008-12-14]
               KM[0.0]HA[0]RU[Japanese]AP[GNU Go:3.7.11]
               (;AW[ja][oa][pa][db][eb] ;AB[fa][ha][ia][qa][cb]))")
         (tree (parse-trees str)))
    (should (= 3 (length tree)))
    (should (= 9 (length (first tree))))
    (should (= 6 (length (car (second tree)))))
    (should (= 6 (length (car (third tree)))))))

(ert-deftest sgf-parse-file-test ()
  (let ((game (read-from-file "sgf-files/jp-ming-5.sgf")))
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
  (let* ((joseki (read-from-file "sgf-files/3-4-joseki.sgf"))
         (root (car joseki))
         (rest (cdr joseki))
         (board (make-board (aget root :S)))
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

(defmacro with-sgf-file (file &rest body)
  (declare (indent 1))
  `(let* ((sgf    (read-from-file ,file))
          (buffer (display-sgf sgf)))
     (unwind-protect (with-current-buffer buffer ,@body)
       (should (kill-buffer buffer)))))
(def-edebug-spec parse-many (file body))

(ert-deftest sgf-display-fresh-sgf-buffer ()
  (with-sgf-file "sgf-files/3-4-joseki.sgf"
    (should local-board)
    (should local-sgf)
    (should local-index)))

(ert-deftest sgf-independent-points-properties ()
  (with-sgf-file "sgf-files/3-4-joseki.sgf"
    (let ((points-length (length (assoc :points (sgf-ref sgf '(0))))))
      (right 4)
      (should (= points-length
                 (length (assoc :points (sgf-ref sgf '(0)))))))))

(ert-deftest sgf-neighbors ()
  (let ((board (make-board 19)))
    (should (= 2 (length (neighbors board 0))))
    (should (= 2 (length (neighbors board (length board)))))
    (should (= 4 (length (neighbors board (/ (length board) 2)))))
    (should (= 3 (length (neighbors board 1))))))

(ert-deftest sgf-singl-stone-capture ()
  (flet ((counts () (cons (stones-for local-board :b)
                          (stones-for local-board :w))))
    (with-sgf-file "sgf-files/1-capture.sgf"
      (right 3) (should (tree-equal (counts) '(2 . 0))))))

(ert-deftest sgf-remove-dead-stone-ko ()
  (flet ((counts () (cons (stones-for local-board :b)
                          (stones-for local-board :w))))
    (with-sgf-file "sgf-files/ko.sgf"
      (should (tree-equal (counts) '(0 . 0))) (right 1)
      (should (tree-equal (counts) '(1 . 0))) (right 1)
      (should (tree-equal (counts) '(1 . 1))) (right 1)
      (should (tree-equal (counts) '(2 . 1))) (right 1)
      (should (tree-equal (counts) '(2 . 2))) (right 1)
      (should (tree-equal (counts) '(3 . 2))) (right 1)
      (should (tree-equal (counts) '(2 . 3))) (right 1)
      (should (tree-equal (counts) '(3 . 2))) (right 1)
      (should (tree-equal (counts) '(2 . 3))))))

(ert-deftest sgf-two-stone-capture ()
  (flet ((counts () (cons (stones-for local-board :b)
                          (stones-for local-board :w))))
    (with-sgf-file "sgf-files/2-capture.sgf"
      (right 8) (should (tree-equal (counts) '(6 . 0))))))

(ert-deftest sgf-parse-empty-properties ()
  (with-sgf-file "sgf-files/w-empty-properties.sgf"
    (should (remove-if-not (lambda (prop)
                             (let ((val (cdr prop)))
                               (and (sequencep val) (= 0 (length val)))))
                           (car sgf)))))

(ert-deftest sgf-paren-matching ()
  (let ((str "(a (b) [c \\] ) ] d)"))
    (should (= (closing-paren str) (length str)))
    (should (= (closing-paren str 3) 6))))
