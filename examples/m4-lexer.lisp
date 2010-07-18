;;;; graylex - cl-m4-excerpt.lisp
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

(in-package :cl-m4)

;;; This file contains modified code from cl-m4 that demonstrates how graylex
;;; can be used.

(defclass m4-input-stream (lexer-input-stream)
  ((macro-stack :accessor m4-macro-stack
                :initform nil)))

(defgeneric m4-push-macro (m4-input-stream macro)
  (:method ((stream m4-input-stream) macro)
    (push macro (m4-macro-stack stream))))

(defgeneric m4-pop-macro (m4-input-stream)
  (:method ((stream m4-input-stream))
    (pop (m4-macro-stack stream))))

(defmethod stream-read-token :around ((stream m4-input-stream) &optional (peek nil))
  (declare (ignore peek))
  (if (m4-macro-stack stream)
      (values :macro-token (m4-pop-macro stream))
    (call-next-method)))

;; dynamic variables
(defparameter *m4-lib* (make-hash-table :test #'equal))
(defvar *m4-quote-start*)
(defvar *m4-quote-end*)
(defvar *m4-comment-start*)
(defvar *m4-comment-end*)
(defvar *m4-macro-name*)

(defun process-m4 (input-stream output-stream &key (include-path (list)) (prepend-include-path (list)))
  (let* ((*m4-quote-start* "`")
         (*m4-quote-end* "'")
         (*m4-comment-start* "#")
         (*m4-comment-end* "\\n")
         (*m4-macro-name* "[_a-zA-Z]\\w*")
         (*m4-wrap-stack* (list))
         (*m4-include-path* (append (reverse prepend-include-path) (list ".") include-path))
         (*m4-diversion* 0)
         (*m4-diversion-table* (make-m4-diversion-table output-stream))
         (lexer (make-instance 'm4-input-stream
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
    (with-m4-lib (parse-m4 lexer))))

