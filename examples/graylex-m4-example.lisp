;;;; graylex - graylex-m4-example.lisp
;;;; Copyright (C) 2010  Alexander Kahl <e-user@fsfe.org>
;;;; This file is part of graylex.
;;;; graylex is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; graylex is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :graylex-m4-example)

;;; This file contains modified code from cl-m4 that demonstrates how graylex
;;; can be used. If you want a full-fledged example including recursive descent
;;; parsing, please checkout http://github.com/e-user/cl-m4

;; heredoc init
(eval-when (:compile-toplevel :execute :load-toplevel)
  (set-dispatch-macro-character #\# #\> #'cl-heredoc:read-heredoc))

;; dynamic variables
(defvar *m4-quote-start*)
(defvar *m4-quote-end*)
(defvar *m4-comment-start*)
(defvar *m4-comment-end*)
(defvar *m4-macro-name*)

(defun dump-m4-tokens (lexer)
  (do* ((token (multiple-value-list (stream-read-token lexer))
               (multiple-value-list (stream-read-token lexer)))
        (class (car token) (car token))
        (image (cadr token) (cadr token)))
      ((null class))
    (format t "~a:~a " class
            (cond ((equal :quote-start class)
                   (prog1 image ; normally you'd call the next production rule function here
                     (setq *m4-quote-start* "\\[")))
                  ((equal :quote-end class)
                   (prog1 image
                     (setq *m4-quote-end* "\\]")))
                  ((equal :comment-start class)
                   (prog1 image
                     (setq *m4-comment-start* "/\\*")))
                  ((equal :comment-end class)
                   (prog1 image
                     (setq *m4-comment-end* "\\*/")))
                  ((equal :macro-name class)
                   (prog1 image
                     (when (string= "foo" image)
                       (setq *m4-macro-name* "\\d+"))))
                  (t image)))))

(defun process-m4 (input-stream output-stream)
  (let* ((*m4-quote-start* "`")
         (*m4-quote-end* "'")
         (*m4-comment-start* "#")
         (*m4-comment-end* "\\n")
         (*m4-macro-name* "[_a-zA-Z]\\w*")
         (lexer (make-instance 'lexer-input-stream
                               :stream input-stream
                               :rules '((*m4-comment-start* . :comment-start)
                                        (*m4-comment-end* . :comment-end)
                                        (*m4-macro-name* . :macro-name)
                                        (*m4-quote-start* . :quote-start)
                                        (*m4-quote-end* . :quote-end)
                                        ("," . :comma)
                                        ("\\n" . :newline)
                                        (" " . :space)
                                        ("\\(" . :open-paren)
                                        ("\\)" . :close-paren)
                                        ("." . :token)))))
    (dump-m4-tokens lexer)))

(defun tryme ()
  (let ((example-text
#>eof>This ought to demonstrate how tokenization works using graylex.
After encountering the first `quoted string', quoting works [like this].
# Same goes for comments; suddenly, you'll need
/* C-style comments */
And after evaluating some magic macro like foo, macros are suddenly identified
by integers like 42.eof))
    (format t "~a~%~%" example-text)
    (with-input-from-string (stream example-text)
      (process-m4 stream *standard-output*))))

(format t "Explore ~a and execute GRAYLEX-M4-EXAMPLE:TRYME to get a demonstration~%"
        (asdf:system-relative-pathname 'graylex-m4-example "examples/graylex-m4-example.lisp"))
