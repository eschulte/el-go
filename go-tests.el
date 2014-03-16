;;; go-tests --- tests for varied GO functionality

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
(require 'ert)
(require 'go)


;;; sgf2el tests
(ert-deftest go-sgf-parse-simple-tree ()
  (let* ((str "(;GM[1]FF[4]
               SZ[19]
               GN[GNU Go 3.7.11 load and print]
               DT[2008-12-14]
               KM[0.0]HA[0]RU[Japanese]AP[GNU Go:3.7.11]AW[ja][oa]
               [pa][db][eb])")
         (sgf (sgf2el-str-to-el str)))
    (should (= 1  (length sgf)))
    (should (= 10 (length (first sgf))))
    (should (= 6  (length (car (last (first sgf))))))))

(ert-deftest go-sgf-parse-nested-tree ()
  (let* ((str "(;GM[1]FF[4]
               SZ[19]
               GN[GNU Go 3.7.11 load and print]
               DT[2008-12-14]
               KM[0.0]HA[0]RU[Japanese]AP[GNU Go:3.7.11]
               (;AW[ja][oa][pa][db][eb] ;AB[fa][ha][ia][qa][cb]))")
         (sgf (sgf2el-str-to-el str)))
    (should (= 2 (length sgf)))
    (should (= 9 (length (first sgf))))
    (should (= 2 (length (second sgf))))
    (should (= 6 (length (car (first (second sgf))))))
    (should (= 6 (length (car (second (second sgf))))))))

(ert-deftest go-parse-tree-w-weird-comment ()
  (let* ((str "(;B[kd]N[(c)]LB[ke:a][pf:b]
     C[Black 25 takes larger territory on top, and gives up
     larger territory at the right, as compared to variation
     (a) \\[3-1-2-1-1-1-2-2-3-2-2-1\\].

     White continues with 'a'. White 'b' is an overplay
     that sets up a ladder that ends the game, unless
     White has a ladder break to the lower left.]
     (;W[ke]N[(a)];B[ng];W[ne];B[mf];W[me];B[je];W[kf];B[jf];W[kg];B[jd];W[pf])

     (;W[pf]
     C[White 26 is a mistake unless White has a ladder break to the lower left.]
     N[mistake (b)];B[pe];W[og];B[rg];W[qh];B[ng];W[ne];B[se];W[qg];B[mf];
     W[me];B[lf])
     )")
         (sgf (sgf2el-str-to-el str)))
    (should (= 3 (length sgf)))))

(ert-deftest go-parse-file-test ()
  (let ((sgf (sgf2el-file-to-el "sgf-files/jp-ming-5.sgf")))
    (should (= 247 (length sgf)))))


;;; board tests
(ert-deftest go-empty-board-to-string-test ()
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

(ert-deftest go-non-empty-board-to-string-test ()
  (let* ((joseki (sgf2el-file-to-el "sgf-files/3-4-joseki.sgf"))
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
      (dolist (move moves)
        (apply-move board move)))
    (board-to-string board)
    (should t)))

(ert-deftest go-neighbors ()
  (let ((board (make-board 19)))
    (should (= 2 (length (neighbors board 0))))
    (should (= 2 (length (neighbors board (length board)))))
    (should (= 4 (length (neighbors board (/ (length board) 2)))))
    (should (= 3 (length (neighbors board 1))))))


;;; GTP and gnugo tests
(ert-deftest go-test-char-to-gtp ()
  (should (= 0  (char-to-num ?A)))
  (should (= 7  (char-to-num ?H)))
  (should (= 8  (char-to-num ?J)))
  (should (= 18 (char-to-num ?T)))
  (should (= 0  (char-to-num ?a)))
  (should (= 7  (char-to-num ?h)))
  (should (= 8  (char-to-num ?j)))
  (should (= 18 (char-to-num ?t))))

(defmacro with-gnugo (&rest body)
  `(let (*gnugo*)
     (unwind-protect
         (progn
           (setf *gnugo* (make-instance 'gnugo))
           (setf (buffer *gnugo*) (gnugo-start-process))
           ,@body)
       (let ((kill-buffer-query-functions nil))
         (should (kill-buffer (buffer *gnugo*)))))))

(ert-deftest go-test-gnugo-interaction-through-gtp ()
  (let ((b1 (concat
             "\n"
             "   A B C D E F G H J K L M N O P Q R S T\n"
             "19 . . . . . . . . . . . . . . . . . . . 19\n"
             "18 . . . . . . . . . . . . . . . . . . . 18\n"
             "17 . . . . . . . . . . . . . . . . . . . 17\n"
             "16 . . . + . . . . . + . . . . . + . . . 16\n"
             "15 . . . . . . . . . . . . . . . . . . . 15\n"
             "14 . . . . . . . . . . . . . . . . . . . 14\n"
             "13 . . . . . . . . . . . . . . . . . . . 13\n"
             "12 . . . . . . . . . . . . . . . . . . . 12\n"
             "11 . . . . . . . . . . . . . . . . . . . 11     "
             "WHITE (O) has captured 0 stones\n"
             "10 . . . + . . . . . + . . . . . + . . . 10     "
             "BLACK (X) has captured 0 stones\n"
             " 9 . . . . . . . . . . . . . . . . . . . 9\n"
             " 8 . . . . . . . . . . . . . . . . . . . 8\n"
             " 7 . . . . . . . . . . . . . . . . . . . 7\n"
             " 6 . . . . . . . . . . . . . . . . . . . 6\n"
             " 5 . . . . . . . . . . . . . . . . . . . 5\n"
             " 4 . . . + . . . . . + . . . . . + . . . 4\n"
             " 3 . . . . . . . . . . . . . . . . . . . 3\n"
             " 2 . . . . . . . . . . . . . . . . . . . 2\n"
             " 1 . . . . . . . . . . . . . . . . . . . 1\n"
             "   A B C D E F G H J K L M N O P Q R S T"))
        (b2 (concat
             "\n"
             "   A B C D E F G H J K L M N O P Q R S T\n"
             "19 . . . . . . . . . . . . . . . . . . . 19\n"
             "18 . . . . . . . . . . . . . . . . . . . 18\n"
             "17 . . . . . . . . . . . . . . . . . . . 17\n"
             "16 . . . + . . . . . + . . . . . + . . . 16\n"
             "15 . . . . . . . . . . . . . . . . . . . 15\n"
             "14 . . . . . . . . . . . . . . . . . . . 14\n"
             "13 . . . . . . . . . . . . . . . . . . . 13\n"
             "12 . . . . . . . . . . . . . . . . . . . 12\n"
             "11 . . . . . . . . . . . . . . . . . . . 11     "
             "WHITE (O) has captured 0 stones\n"
             "10 . . . + . . . . . + . . . . . + . . . 10     "
             "BLACK (X) has captured 0 stones\n"
             " 9 . . . . . . . . . . . . . . . . . . . 9\n"
             " 8 . . . . . . . . . . . . . . . . . . . 8\n"
             " 7 . . . . . . . . . . . . . . . . . . . 7\n"
             " 6 . . . . . . . . . . . . . . . . . . . 6\n"
             " 5 . . . . . . . . . . . . . . . . . . . 5\n"
             " 4 . . . + . . . . . + . . . . . + . . . 4\n"
             " 3 . . . . . . . . . . . . . . . . . . . 3\n"
             " 2 X . . . . . . . . . . . . . . . . . . 2\n"
             " 1 X . . . . . . . . . . . . . . . . . . 1\n"
             "   A B C D E F G H J K L M N O P Q R S T")))
    (with-gnugo
     (should (string= b1 (gtp-command *gnugo* "showboard")))
     (should (string= "" (gtp-command *gnugo* "black A1")))
     (should (string= "" (setf (go-move *gnugo*) '(:B :pos . (0 . 1)))))
     (should (string= b2 (gtp-command *gnugo* "showboard"))))))


;;; SGF tests
(defmacro with-sgf-from-file (file &rest body)
  (declare (indent 1))
  `(let (*sgf*)
     (progn
       (setf *sgf* (make-instance 'sgf
                     :self (sgf2el-file-to-el ,file)))
       ,@body)))

(ert-deftest go-parse-empty-properties ()
  (with-sgf-from-file "sgf-files/w-empty-properties.sgf"
    (should (remove-if-not (lambda (prop)
                             (let ((val (cdr prop)))
                               (and (sequencep val) (= 0 (length val)))))
                           (root *sgf*)))))

(ert-deftest go-test-sgf-class-creation ()
  (with-sgf-from-file "sgf-files/jp-ming-5.sgf"
    (should (tree-equal (index *sgf*) '(0)))
    (should (tree-equal (current *sgf*) (root *sgf*)))
    (should (string= "Famous Blood Vomiting Game" (go-name *sgf*)))
    (should (= 19 (go-size *sgf*)))))


;;; SGF and board tests
(defmacro with-sgf-display (file &rest body)
  (declare (indent 1))
  (let ((buffer (gensym "sgf-display-buffer")))
    `(let ((,buffer (go-board
                     (make-instance 'sgf
                       :self (sgf2el-file-to-el ,file)
                       :index '(0)))))
       (unwind-protect (with-current-buffer ,buffer
                         (setf (index *back-end*) (list 0))
                         ,@body)
         (should (kill-buffer ,buffer))))))
(def-edebug-spec parse-many (file body))

(ert-deftest go-display-fresh-go-buffer ()
  (with-sgf-display "sgf-files/3-4-joseki.sgf"
    (should *history*)
    (should *back-end*)))

(ert-deftest go-independent-points-properties ()
  (with-sgf-display "sgf-files/3-4-joseki.sgf"
    (go-board-next 4)
    (should (not (tree-equal (car *history*) (car (last *history*)))))))

(defun go-stone-counts ()
  (let ((pieces (car *history*)))
    (cl-flet ((count-for (color) (length (remove-if-not
                                      (lambda (piece) (equal color (car piece)))
                                      pieces))))
      (cons (count-for :B) (count-for :W)))))

(ert-deftest go-singl-stone-capture ()
  (with-sgf-display "sgf-files/1-capture.sgf"
    (go-board-next 3) (should (tree-equal (go-stone-counts) '(2 . 0)))))

(ert-deftest go-remove-dead-stone-ko ()
  (with-sgf-display "sgf-files/ko.sgf"
    (should (tree-equal (go-stone-counts) '(0 . 0))) (go-board-next)
    (should (tree-equal (go-stone-counts) '(1 . 0))) (go-board-next)
    (should (tree-equal (go-stone-counts) '(1 . 1))) (go-board-next)
    (should (tree-equal (go-stone-counts) '(2 . 1))) (go-board-next)
    (should (tree-equal (go-stone-counts) '(2 . 2))) (go-board-next)
    (should (tree-equal (go-stone-counts) '(3 . 2))) (go-board-next)
    (should (tree-equal (go-stone-counts) '(2 . 3))) (go-board-next)
    (should (tree-equal (go-stone-counts) '(3 . 2))) (go-board-next)
    (should (tree-equal (go-stone-counts) '(2 . 3)))))

(ert-deftest go-two-stone-capture ()
  (with-sgf-display "sgf-files/2-capture.sgf"
    (go-board-next 8) (should (tree-equal (go-stone-counts) '(6 . 0)))))

(ert-deftest go-connect-gnugo-to-sgf ()
  (let ((*sgf* (make-instance 'sgf))
        (*gnugo* (make-instance 'gnugo :buffer (gnugo-start-process))))
    (unwind-protect
        (progn (setf (go-size *sgf*) (go-size *gnugo*))
               (should (= (go-size *sgf*) (go-size *gnugo*)))
               (setf (go-name *sgf*) (go-name *gnugo*))
               (should (string= (go-name *sgf*) (go-name *gnugo*)))
               (dotimes (n 5) (setf (go-move *sgf*) (go-move *gnugo*))))
      (let ((kill-buffer-query-functions nil))
        (should (kill-buffer (buffer *gnugo*)))))))

(provide 'go-tests)
;;; go-tests.el ends here
