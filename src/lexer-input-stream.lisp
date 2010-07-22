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

(define-condition unmatched-lexing-sequence (error)
  ((sequence :initarg :sequence
             :reader  unmatched-sequence
             :documentation "Copy of the unmatchable sequence")
   (row      :initarg :row
             :reader  unmatched-sequence-row
             :documentation "Row part of unmatching position")
   (column   :initarg :column
             :reader  unmatched-sequence-column
             :documentation "Column part of unmatching position"))
  (:documentation "Condition signaling that no lexer rule matches."))

(defclass lexer-input-stream (buffered-input-stream)
  ((rules :initarg :rules
          :accessor lexer-rules
          :initform nil
          :documentation "List of regexp/keyword conses")
   (row   :accessor lexer-row
          :initform 1
          :documentation "Current row in lexer stream")
   (column :accessor lexer-column
           :initform 0
           :documentation "Current column in lexer stream")
   (non-stream-position :accessor lexer-non-stream-position
                        :initform 0
                        :documentation "Position in unread sequence")
   (double-buffer :accessor lexer-double-buffer
                  :initform ""
                  :documentation "Double buffer"))
  (:documentation "Lexer input streams provide lexical analysis, tracking of
input row and column and a dynamic second buffer for input tokens longer than
the primary BUFFERED-INPUT-STREAM buffer size."))

(defgeneric lexer-unread-sequence (lexer-input-stream seq)
  (:documentation "Unread a sequence by feeding it into the double buffer")
  (:method ((stream lexer-input-stream) seq)
    "lexer-unread-sequence stream seq => position

Prepend sequence SEQ to the internal double buffer and increase the non-stream
position."
    (with-accessors ((double-buffer lexer-double-buffer)
                     (position lexer-non-stream-position))
        stream
      (setq double-buffer (concatenate 'string seq double-buffer))
      (incf position (length seq)))))

(defmethod flush-buffer ((stream lexer-input-stream))
  "flush-buffer stream => string

Return unread rest of the wrapped main buffer but also append it to the double
buffer."
  (with-accessors ((double-buffer lexer-double-buffer))
      stream
    (let ((buffer-contents (call-next-method)))
      (setq double-buffer (concatenate 'string double-buffer buffer-contents))
      buffer-contents)))

(defmethod stream-read-char ((stream lexer-input-stream))
  "stream-read-char stream => char or :eof

Also save read characters into the double buffer."
  (with-accessors ((double-buffer lexer-double-buffer))
      stream
    (let ((char (call-next-method)))
      (prog1 char
        (when (characterp char)
          (setq double-buffer (format nil "~a~c" double-buffer char)))))))

(defgeneric stream-read-token (lexer-input-stream &optional peek)
  (:documentation "Read lexical tokens from the input stream")
  (:method :before ((stream lexer-input-stream) &optional (peek nil))
    "stream-read-token :before stream &optional peek => string

If the internal double buffer is empty, flush the main buffer first in order to
replenish it."
    (declare (ignore peek))
    (when (= 0 (length (lexer-double-buffer stream)))
      (flush-buffer stream)))
  (:method :around ((stream lexer-input-stream) &optional (peek nil))
    "stream-read-token :around stream &optional peek => (class image)

Scan the result from calling the next method if PEEK is NIL:
Discard the matched part from the beginning of the double buffer and either just
decrease the non-stream position or record the column and row progress."
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
    "stream-read-token stream &optional peek => (class image)

Scan the lexer's double buffer successively with all its rules. Rules are
expected to be conses of PCRE-compatible regular expressions and class name
keywords. Heads-up: Every rule get prepended with an implicit \"^\" to match the
beginning of the buffer!
If the double buffer is empty, simply return NIL; is no rule matches, signal an
UNMATCHED-LEXING-SEQUENCE with further details and provide the following
restarts:
- flush-buffer: Call the method of the same name and try to scan again
- skip-characters count: Skip COUNT characters of the reported sequence and try
  to scan again

Be sure to *not* use any of these unconditionally, you'll end up with an
infinite loop! Instead, apply UNMATCHED-SEQUENCE to the condition in your
handler to investigate and act accordingly; e.g. a hypothetical lexer rule could
require at least five characters to match but the unmatched sequence has only
three so reasonable handling code could look like this:
> (handler-bind ((unmatched-lexing-sequence #'(lambda (condition)
                                                (if (< (length (unmatched-sequence condition)) 5)
                                                    (invoke-restart 'flush-buffer)
                                                  (error condition)))))
    (function-that-invokes-stream-read-token))"
    (declare (ignore peek))
    (with-accessors ((double-buffer lexer-double-buffer))
        stream
      (labels ((scan (chunk rules)
                 (restart-case
                     (or (some #'(lambda (pair)
                                   (let ((match (cl-ppcre:scan-to-strings (concatenate 'string "^" (eval (car pair))) double-buffer)))
                                     (when (and match (> (length match) 0)) ; zero-length matches are not allowed
                                       (if (= (length match) (length chunk))
                                           (scan (flush-buffer stream) (list pair))
                                         (list (cdr pair) match)))))
                               rules)
                         (error 'unmatched-lexing-sequence
                                :sequence chunk
                                :row (lexer-row stream)
                                :column (lexer-column stream)))
                   (flush-buffer ()
                     (flush-buffer stream)
                     (scan chunk rules))
                   (skip-characters (count)
                     (setq chunk (subseq chunk count))
                     (scan chunk rules)))))
        (when (> (length double-buffer) 0)
          (apply #'values (scan double-buffer (lexer-rules stream))))))))
