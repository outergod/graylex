;;;; graylex - lexer-input-stream
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
(defsuite lexer)
(in-suite lexer)

(deftest lexer-flushing (stream expected-unread expected-buffer expected-double-buffer)
  (is (string= expected-unread (flush-buffer stream)))
  (is (string= expected-buffer (buffered-input-buffer stream)))
  (is (string= expected-double-buffer (lexer-double-buffer stream))))

(deftest lexer-reading (stream expected-char expected-position expected-buffer expected-double-buffer)
  (is (string= (string expected-char) (stream-read-char stream)))
  (is (= expected-position (buffered-input-position stream)))
  (is (string= expected-buffer (buffered-input-buffer stream)))
  (is (string= expected-double-buffer (lexer-double-buffer stream))))

(deftest lexer-unreading (stream expected-position expected-double-buffer)
  (is (= expected-position (lexer-non-stream-position stream)))
  (is (string= expected-double-buffer (lexer-double-buffer stream))))


(deftest lexer-simple-flushing ()
  (with-input-from-string (stream "abcdefghij")
    (let ((lexer-stream (make-instance 'lexer-input-stream
                                       :stream stream
                                       :buffer-size 3)))
      (lexer-flushing lexer-stream "abc" "def" "abc")
      (lexer-flushing lexer-stream "def" "ghi" "abcdef")
      (lexer-flushing lexer-stream "ghi" "j" "abcdefghi")
      (lexer-flushing lexer-stream "j" "" "abcdefghij")
      (lexer-flushing lexer-stream "" "" "abcdefghij"))))

(deftest lexer-simple-reading ()
  (with-input-from-string (stream "abcdefghij")
    (let ((lexer-stream (make-instance 'lexer-input-stream
                                       :stream stream
                                       :buffer-size 3)))
      (lexer-reading lexer-stream #\a 1 "abc" "a")
      (lexer-reading lexer-stream #\b 2 "abc" "ab")
      (lexer-reading lexer-stream #\c 3 "abc" "abc")
      (lexer-reading lexer-stream #\d 1 "def" "abcd")
      (lexer-reading lexer-stream #\e 2 "def" "abcde")
      (lexer-reading lexer-stream #\f 3 "def" "abcdef")
      (lexer-reading lexer-stream #\g 1 "ghi" "abcdefg")
      (lexer-reading lexer-stream #\h 2 "ghi" "abcdefgh")
      (lexer-reading lexer-stream #\i 3 "ghi" "abcdefghi")
      (lexer-reading lexer-stream #\j 1 "j" "abcdefghij")
      (lexer-reading lexer-stream :eof 0 "" "abcdefghij"))))

;; (deftest sequence-reading ()
;;   (with-input-from-string (stream "hooray!")
;;     (let ((buffered-stream (make-instance 'buffered-input-stream
;;                                           :stream stream
;;                                           :buffer-size 3))
;;           (sequence (make-array 9 :element-type 'character :adjustable nil)))
;;       (setf (char sequence 0) #\*
;;             (char sequence 8) #\*)
;;       (stream-read-sequence buffered-stream sequence 1 8)
;;       (is (string= "*hooray!*" sequence)))))
