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

(defclass buffered-input-stream (trivial-gray-streams:fundamental-character-input-stream)
   ((stream :initarg :stream
            :reader buffered-stream
            :initform (alexandria:required-argument :stream))
    (buffer-size :initarg :buffer-size
                 :accessor buffered-input-size
                 :initform 1024)
    (buffer-position :accessor buffered-input-position
                     :initform 0)
    (buffer :accessor buffered-input-buffer)))

(defmethod close ((stream buffered-input-stream) &key abort)
  (close (buffered-stream stream) :abort abort))

(defgeneric flush-buffer (buffered-input-stream)
  (:method ((stream buffered-input-stream))
    (prog1 (subseq (buffered-input-buffer stream) (buffered-input-position stream))
      (fill-buffer stream))))

(defmethod initialize-instance :after ((stream buffered-input-stream)  &rest initargs)
  (declare (ignore initargs))
  (with-accessors ((size buffered-input-size))
      stream
    (setf (buffered-input-buffer stream)
          (make-array (buffered-input-size stream)
                      :element-type 'character :adjustable nil :fill-pointer size))
    (fill-buffer stream)))

(defgeneric fill-buffer (buffered-input-stream)
  (:method ((stream buffered-input-stream))
    (with-accessors ((buffer buffered-input-buffer))
        stream
      (setf (fill-pointer buffer) (read-sequence buffer (buffered-stream stream))
            (buffered-input-position stream) 0))))

(defmethod trivial-gray-streams:stream-read-char :before ((stream buffered-input-stream))
  (with-accessors ((buffer buffered-input-buffer)
                   (position buffered-input-position))
      stream
    (when (>= position (fill-pointer buffer))
      (fill-buffer stream))))

(defmethod trivial-gray-streams:stream-read-char ((stream buffered-input-stream))
  (with-accessors ((buffer buffered-input-buffer)
                   (position buffered-input-position))
      stream
    (if (= 0 (fill-pointer buffer))
        :eof
      (prog1 (char buffer position)
        (incf position)))))

(defmethod trivial-gray-streams:stream-unread-char ((stream buffered-input-stream) char)
  (error "Unreading chars is not supported for buffered-input-streams"))

(defmethod trivial-gray-streams:stream-read-sequence ((stream buffered-input-stream) seq start end &key)
  (read-sequence seq stream :start start :end end))
