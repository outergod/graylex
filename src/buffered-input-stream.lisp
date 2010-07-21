;;;; graylex - buffered-input-stream.lisp
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

(defclass buffered-input-stream (fundamental-character-input-stream trivial-gray-stream-mixin)
   ((stream :initarg :stream
            :reader buffered-stream
            :initform (alexandria:required-argument :stream)
            :documentation "Wrapped input stream that is to be buffered")
    (buffer-size :initarg :buffer-size
                 :accessor buffered-input-size
                 :initform 1024
                 :documentation "Size of the input buffer")
    (buffer-position :accessor buffered-input-position
                     :initform 0
                     :documentation "Reading position of the wrapped buffer")
    (buffer :accessor buffered-input-buffer
            :documentation "Wrapped input buffer"))
   (:documentation "Base class for buffered input streams. The default buffer
size is 1024 bytes."))

(defmethod close ((stream buffered-input-stream) &key abort)
  (close (buffered-stream stream) :abort abort))

(defgeneric flush-buffer (buffered-input-stream)
  (:documentation "Flush the input buffer")
  (:method ((stream buffered-input-stream))
    "flush-buffer stream => string

Return unread rest of the wrapped buffer and replenish it."
    (with-accessors ((position buffered-input-position)
                     (buffer buffered-input-buffer))
        stream
      (prog1 (subseq buffer position)
        (fill-buffer stream)))))

(defmethod initialize-instance :after ((stream buffered-input-stream) &rest initargs)
  "initialize-instance :after stream &rest initargs => position

Create the input buffer after initialization and fill it for the first time."
  (declare (ignore initargs))
  (with-accessors ((size buffered-input-size)
                   (buffer buffered-input-buffer))
      stream
    (setq buffer (make-array size :element-type 'character :adjustable nil :fill-pointer size))
    (fill-buffer stream)))

(defgeneric fill-buffer (buffered-input-stream)
  (:documentation "Fill the input buffer")
  (:method ((stream buffered-input-stream))
    "fill-buffer stream => position

Fill the input buffer by reading from the wrapped stream. Also reset the reading
position to zero."
    (with-accessors ((position buffered-input-position)
                     (buffer buffered-input-buffer))
        stream
      (setf (fill-pointer buffer) (read-sequence buffer (buffered-stream stream))
            position 0))))

(defmethod stream-read-char :before ((stream buffered-input-stream))
  "stream-read-char :before stream => position

If reading beyond the internal buffer, replenish it."
  (with-accessors ((buffer buffered-input-buffer)
                   (position buffered-input-position))
      stream
    (when (>= position (fill-pointer buffer))
      (flush-buffer stream)))) ; we use flush here because children would rather
                               ; implement flush-buffer than fill-bufer

(defmethod stream-read-char ((stream buffered-input-stream))
  "stream-read-char :before stream => char or :eof

Return next character from wrapped input buffer or :EOF if the end of the input
stream is reached."
  (with-accessors ((buffer buffered-input-buffer)
                   (position buffered-input-position))
      stream
    (if (= 0 (fill-pointer buffer))
        :eof
      (prog1 (char buffer position)
        (incf position)))))

(defmethod stream-unread-char ((stream buffered-input-stream) char)
  "stream-unread-char stream char => error

Unreading chars is not supported for BUFFERED-INPUT-STREAMS."
  (error "Unreading chars is not supported for buffered-input-streams"))

(defmethod stream-read-sequence ((stream buffered-input-stream) seq start end &key)
  "stream-read-sequence stream seq start end &key => position

Apply READ-SEQUENCE to given arguments and the wrapped input stream. All
mechanisms of BUFFERED-INPUT-STREAM apply.
Please see READ-SEQUENCE's documentation."
  (read-sequence seq stream :start start :end end))
