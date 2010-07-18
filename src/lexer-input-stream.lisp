;;;; graylex - lexer-input-stream.lisp
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

(in-package :graylex)

(defclass lexer-input-stream (buffered-input-stream)
  ((rules :initarg :rules
          :accessor lexer-rules
          :initform nil)
   (row   :accessor lexer-row
          :initform 1)
   (column :accessor lexer-column
           :initform 0)
   (non-stream-position :accessor lexer-non-stream-position
                        :initform 0)
   (double-buffer :accessor lexer-double-buffer
                  :initform "")))

(defgeneric lexer-unread-sequence (lexer-input-stream seq)
  (:method ((stream lexer-input-stream) seq)
    (with-accessors ((double-buffer lexer-double-buffer)
                     (position lexer-non-stream-position))
        stream
      (setq double-buffer (concatenate 'string seq double-buffer))
      (incf position (length seq)))))

(defmethod flush-buffer ((stream lexer-input-stream))
  (with-accessors ((double-buffer lexer-double-buffer))
      stream
    (let ((buffer-contents (call-next-method)))
      (setq double-buffer (concatenate 'string double-buffer buffer-contents))
      buffer-contents)))

(defgeneric stream-read-token (lexer-input-stream &optional peek)
  (:method :before ((stream lexer-input-stream) &optional (peek nil))
    (declare (ignore peek))
    (when (= 0 (length (lexer-double-buffer stream)))
      (flush-buffer stream)))
  (:method :around ((stream lexer-input-stream) &optional (peek nil))
    (with-accessors ((double-buffer lexer-double-buffer)
                     (position lexer-non-stream-position))
        stream
      (multiple-value-bind (class image)
          (call-next-method)
        (when (and class (null peek))
          (let ((length (length image)))
            (setq double-buffer (subseq double-buffer length))
            (if (>= position length)
                (decf position length)
              (let ((newlines (count (string #\Newline) image :test #'string=)))
                (if (> newlines 0)
                    (progn
                      (setf (lexer-column stream)
                            (search (string #\Newline) (reverse image)))
                      (incf (lexer-row stream) newlines))
                  (incf (lexer-column stream) (- length position)))))))
        (values class image))))
  (:method ((stream lexer-input-stream) &optional (peek nil))
    (declare (ignore peek))
    (with-accessors ((double-buffer lexer-double-buffer))
        stream
      (labels ((scan (chunk rules)
                 (some #'(lambda (pair)
                           (let ((match (cl-ppcre:scan-to-strings (concatenate 'string "^" (eval (car pair))) double-buffer)))
                             (when (and match (> (length match) 0)) ; zero-length matches are not allowed
                               (if (= (length match) (length chunk))
                                   (scan (flush-buffer stream) (list pair))
                                 (list (cdr pair) match)))))
                       rules)))
        (apply #'values (scan double-buffer (lexer-rules stream)))))))
