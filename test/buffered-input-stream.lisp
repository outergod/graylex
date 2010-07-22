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

(in-package :graylex-test)

(in-suite all)
(defsuite buffer)
(in-suite buffer)

(deftest buffer-simple-flushing ()
  (simple-flushing 'buffered-input-stream))

(deftest buffer-simple-reading ()
  (simple-reading 'buffered-input-stream))

(deftest buffer-sequence-reading ()
  (with-input-from-string (stream "hooray!")
    (let ((buffered-stream (make-instance 'buffered-input-stream
                                          :stream stream
                                          :buffer-size 3))
          (sequence (make-array 9 :element-type 'character :adjustable nil)))
      (setf (char sequence 0) #\*
            (char sequence 8) #\*)
      (stream-read-sequence buffered-stream sequence 1 8)
      (is (string= "*hooray!*" sequence)))))
