;;; sgf2el.el --- conversion between sgf and emacs-lisp

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
(require 'go-util)

(defvar prop-re
  "\\([[:alpha:]]+\\)\\(\\(\\[\\]\\|[[:space:]]*\\[[^\000]*?[^\\]\\]\\)+\\)")

(defvar prop-val-re
  "\\(\\[\\]\\|\\[\\([^\000]*?[^\\]\\)\\]\\)")

(defvar sgf2el-special-properties nil
  "A-list of properties and functions to specially convert their values.")

(defun make-keyword (string)
  (intern (concat ":" (upcase string))))

(defun sgf2el-convert-prop-key (key)
  "Convert a keyerty name to elisp."
  (save-match-data (make-keyword key)))

(defun sgf2el-read-prop (val)
  (when (and (stringp val) (not (equal val "")))
    (or (go-number-p val) val)))

(defun sgf2el-convert-prop-vals (key vals)
  "Convert a property value to elisp."
  (save-match-data
    (let ((func (cdr (assoc key sgf2el-special-properties))))
      (if func
          (funcall func vals)
        (delete nil (mapcar #'sgf2el-read-prop vals))))))

(defun sgf2el-all-matches (str re &optional sub-exp)
  (save-match-data
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (loop while (re-search-forward re nil t)
            collect (go-clean-text-properties
                     (match-string (or sub-exp 0)))))))

(defun sgf2el-region (&optional start end)
  (interactive "r")
  (let ((start (copy-marker (or start (point-min))))
        (end   (copy-marker (or end   (point-max))))
        (re    (format "\\(%s\\|%s\\)" prop-re "\\(([[:space:]]*\\)*\\(;\\)"))
        last-node)
    (save-excursion (goto-char start)
      (while (re-search-forward re end t)
        (let ((start (marker-position start)))
          (message "parsing %.2f%%"
                   (* 100 (/ (float (- (point) start))
                             (float (- (marker-position end) start))))))
        (if (string= (match-string 6) ";")
            (progn
              (replace-match "(" nil nil nil 6)
              (when last-node
                (save-excursion (goto-char (match-beginning 0)) (insert ")")))
              (setq last-node t))
          (let* ((key (sgf2el-convert-prop-key (match-string 2)))
                 (val (sgf2el-convert-prop-vals key
                       (sgf2el-all-matches (match-string 3) prop-val-re 2)))
                 (rep (format "%S " (cons key (if (= 1 (length val))
                                                  (car val) val)))))
            (replace-match rep nil 'literal))))
      (when last-node (insert ")")))
    (message "parsing DONE")))

(defun sgf2el-normalize (&optional buffer)
  "Cleanup the formatting of the elisp sgf data in BUFFER."
  (interactive)
  (let ((buffer (or buffer (current-buffer))) temp)
    (sgf2el-set-to-var temp buffer)
    (with-current-buffer buffer
      (save-excursion
        (delete-region (point-min) (point-max))
        (insert (pp temp))))
    temp))

(defun sgf2el (&optional sgf-buffer)
  "Convert the content of SGF-BUFFER to emacs-lisp in a new buffer."
  (interactive)
  (let* ((sgf-buffer (or sgf-buffer (current-buffer)))
         (buffer (generate-new-buffer (concat (buffer-name sgf-buffer) "-el")))
         (sgf-str (with-current-buffer sgf-buffer (buffer-string))))
    (with-current-buffer buffer
      (insert sgf-str)
      (goto-char (point-min))
      (sgf2el-region)
      (emacs-lisp-mode))
    (pop-to-buffer buffer)))

(defun sgf2el-read (&optional buf)
  (with-current-buffer (or buf (current-buffer))
    (goto-char (point-min))
    (read (current-buffer))))

(defun sgf2el-buffer-to-el (&optional bufffer)
  "Convert the sgf contents of BUFFER to emacs lisp."
  (interactive "b")
  (with-current-buffer (or bufffer (current-buffer))
    (sgf2el-region (point-min) (point-max))
    (sgf2el-read)))

(defun sgf2el-str-to-el (str)
  "Convert a string of sgf into the equivalent Emacs Lisp."
  (interactive)
  (with-temp-buffer (insert str) (sgf2el-buffer-to-el)))

(defun sgf2el-file-to-el (file)
  "Convert the sgf contents of FILE to emacs lisp."
  (interactive "f")
  (with-temp-buffer
    (insert-file-contents-literally file)
    (sgf2el-buffer-to-el)))


;;; Specific property converters
(defun process-date (date-args)
  (save-match-data (parse-time-string
                    (if (> 1 (length date-args))
                        (mapconcat #'number-to-string date-args " ")
                      (car date-args)))))
(add-to-list 'sgf2el-special-properties (cons :DT #'process-date))

(defun process-position (position-string)
  (cl-flet ((char-to-num (char)
                      (cond
                       ((or (< char ?A) (< ?z char))
                        (error "sgf: invalid char %s" char))
                       ((< char ?a) (+ 26 (- char ?A)))
                       (t           (- char ?a)))))
    (cons (char-to-num (aref position-string 0))
          (char-to-num (aref position-string 1)))))

(defun process-move (move-args)
  (list (cons :pos (process-position (car move-args)))))
(add-to-list 'sgf2el-special-properties (cons :B #'process-move))
(add-to-list 'sgf2el-special-properties (cons :W #'process-move))

(defun process-label (label-args)
  (let ((res (mapcar (lambda (l-arg)
                       (if (string-match "\\([[:alpha:]]+\\):\\(.*\\)" l-arg)
                           (list
                            (cons :label (match-string 2 l-arg))
                            (cons :pos (process-position
                                        (match-string 1 l-arg))))
                         (error "sgf: malformed label %S" l-arg)))
                     label-args)))
    (if (= 1 (length label-args)) (list res) res)))
(add-to-list 'sgf2el-special-properties (cons :LB #'process-label))
(add-to-list 'sgf2el-special-properties (cons :LW #'process-label))

(defun process-comment (comments)
  (let ((replacements '(("\\(" . "(")
                        ("\\)" . ")")
                        ("\\[" . "[")
                        ("\\]" . "]"))))
    (mapcar (lambda (comment)
              (dolist (pair replacements comment)
                (setq comment (replace-regexp-in-string
                               (regexp-quote (car pair)) (cdr pair) comment))))
            comments)))
(add-to-list 'sgf2el-special-properties (cons :C #'process-comment))

(provide 'sgf2el)
;;; sgf2el.el ends here
