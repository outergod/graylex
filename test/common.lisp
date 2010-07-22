;;;; graylex - common.lisp
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

(defparameter *sequence-fixture* nil)
(defixture sequence-fixture
  (:setup (setq *sequence-fixture*
                (list "abcdefghij"
                      (format nil "This is~%a test")))))

(defmacro with-buffer-input-from-string ((var stream-class buffer-size string) &body body)
  (let ((stream (gensym)))
    `(with-input-from-string (,stream ,string)
       (let ((,var (make-instance ,stream-class
                                  :stream ,stream
                                  :buffer-size ,buffer-size)))
         ,@body))))

(defun safe-subseq (sequence start &optional (end (length sequence)))
  (let ((length (length sequence)))
    (cond ((> start length) "")
          ((> end length) (subseq sequence start length))
          (t (subseq sequence start end)))))

(deftest buffer-flushing (stream expected-unread expected-buffer &optional expected-double-buffer)
  (is (string= expected-unread (flush-buffer stream)))
  (is (string= expected-buffer (buffered-input-buffer stream)))
  (when expected-double-buffer
    (is (string= expected-double-buffer (lexer-double-buffer stream)))))

(deftest buffer-reading (stream expected-char expected-position expected-buffer &optional expected-double-buffer)
  (is (string= (string expected-char) (stream-read-char stream)))
  (is (= expected-position (buffered-input-position stream)))
  (is (string= expected-buffer (buffered-input-buffer stream)))
  (when expected-double-buffer
    (is (string= expected-double-buffer (lexer-double-buffer stream)))))

(deftest simple-flushing (buffer-class &optional test-double-buffer)
  (with-fixture sequence-fixture
    (mapc #'(lambda (string)
              (dotimes (count 10)
                (let ((buffer-length (1+ count)))
                  (with-buffer-input-from-string (stream buffer-class count string)
                    (let ((steps (ceiling (/ (length string) buffer-length))))
                      (dotimes (step steps)
                        (buffer-flushing stream
                                         (safe-subseq string (* step count) (* (1+ step) count))
                                         (safe-subseq string (* (1+ step) count) (* (+ 2 step) count))
                                         (when test-double-buffer
                                           (safe-subseq string 0 (* (1+ step) count))))))))))
          *sequence-fixture*)))

(deftest simple-reading (buffer-class &optional test-double-buffer)
  (with-fixture sequence-fixture
    (mapc #'(lambda (string)
              (dotimes (count 10)
                (let ((length (length string))
                      (buffer-length (1+ count)))
                  (with-buffer-input-from-string (stream buffer-class buffer-length string)
                    (do* ((step 0 (1+ step))
                          (index 1 (if (>= step length)
                                       0
                                     (1+ (mod step buffer-length))))
                          (char (schar string 0)
                                (if (>= step length)
                                    :eof
                                  (schar string step)))
                          (chunk 0 (floor step buffer-length)))
                        ((= step (1+ length)))
                      (buffer-reading stream char index
                                      (if (>= step length)
                                          ""
                                        (safe-subseq string
                                                     (* chunk buffer-length)
                                                     (+ buffer-length (* chunk buffer-length))))
                                        (when test-double-buffer
                                           (safe-subseq string 0 (1+ step)))))))))
          *sequence-fixture*)))
