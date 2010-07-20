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

(deftest flushing (stream expected-unread expected-buffer)
  (is (string= expected-unread (flush-buffer stream)))
  (is (string= expected-buffer (buffered-input-buffer stream))))

(deftest reading (stream expected-char expected-position expected-buffer)
  (is (string= (string expected-char) (stream-read-char stream)))
  (is (= expected-position (buffered-input-position stream)))
  (is (string= expected-buffer (buffered-input-buffer stream))))

(deftest simple-flushing ()
  (with-input-from-string (stream "abcdefghij")
    (let ((buffered-stream (make-instance 'buffered-input-stream
                                          :stream stream
                                          :buffer-size 3)))
      (flushing buffered-stream "abc" "def")
      (flushing buffered-stream "def" "ghi")
      (flushing buffered-stream "ghi" "j")
      (flushing buffered-stream "j" ""))))

(deftest simple-reading ()
  (with-input-from-string (stream "abcdefghij")
    (let ((buffered-stream (make-instance 'buffered-input-stream
                                          :stream stream
                                          :buffer-size 3)))
      (reading buffered-stream #\a 1 "abc")
      (reading buffered-stream #\b 2 "abc")
      (reading buffered-stream #\c 3 "abc")
      (reading buffered-stream #\d 1 "def")
      (reading buffered-stream #\e 2 "def")
      (reading buffered-stream #\f 3 "def")
      (reading buffered-stream #\g 1 "ghi")
      (reading buffered-stream #\h 2 "ghi")
      (reading buffered-stream #\i 3 "ghi")
      (reading buffered-stream #\j 1 "j")
      (reading buffered-stream :eof 0 ""))))

(deftest sequence-reading ()
  (with-input-from-string (stream "hooray!")
    (let ((buffered-stream (make-instance 'buffered-input-stream
                                          :stream stream
                                          :buffer-size 3))
          (sequence (make-array 9 :element-type 'character :adjustable nil)))
      (setf (char sequence 0) #\*
            (char sequence 8) #\*)
      (stream-read-sequence buffered-stream sequence 1 8)
      (is (string= "*hooray!*" sequence)))))
