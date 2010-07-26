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

(deftest lexer-simple-flushing ()
  (simple-flushing 'lexer-input-stream t))

(deftest lexer-simple-reading ()
  (simple-reading 'lexer-input-stream t))

(deftest lexer-sequence-reading ()
  (simple-sequence-reading 'lexer-input-stream t))

(deftest sequence-unreading (stream expected-position expected-double-buffer)
  (is (= expected-position (lexer-non-stream-position stream)))
  (is (string= expected-double-buffer (lexer-double-buffer stream))))

(deftest token-reading (stream expected-token-length expected-token-class)
  (do ((step (- expected-token-length (length (lexer-double-buffer stream))) (1- step)))
      ((< step 0))
    (when (eql :eof (read-char stream nil :eof))
      (decf expected-token-length)
      (setq expected-token-class :catchall)))
  (let ((column (lexer-column stream))
        (row (lexer-row stream))
        (expected-token (cadr (multiple-value-list (stream-read-token stream t)))))
    (is (= column (lexer-column stream))) ; test for peek side-effects
    (is (= row (lexer-row stream)))
    (multiple-value-bind (class image)
        (stream-read-token stream)
      (when (> (length (lexer-double-buffer stream)) 0)
        (is (= expected-token-length (length image)))
        (is (eql expected-token-class class)))
      (is (string= expected-token image))
      (mapc #'(lambda (char)
                (if (char= #\Newline char)
                    (progn
                      (incf row)
                      (setq column 0))
                  (incf column)))
            (coerce image 'list))
      (is (= column (lexer-column stream)))
      (is (= row (lexer-row stream))))))

(deftest lexer-sequence-unreading ()
  (with-buffer-input-from-string (stream 'lexer-input-stream 3 "bar")
    (sequence-unreading stream 0 "")
    (lexer-unread-sequence stream "foo")
    (sequence-unreading stream 3 "foo")
    (buffer-flushing stream "bar" "" "foobar")))

(deftest lexer-token-reading ()
  (map-sequence-fixtures #'(lambda (string)
                             (do* ((variant 0 (1+ variant))
                                   (buffer-length 1 (1+ (floor (/ variant 3))))
                                   (token-length 1 (1+ (mod variant 3))))
                                 ((= 9 variant))
                               (with-buffer-input-from-string (stream 'lexer-input-stream buffer-length string
                                                                      :rules (list (cons (format nil "(?:.|\\n){~d}" token-length)
                                                                                         :token)
                                                                                   (cons "." :catchall)))
                                 (let ((steps (ceiling (/ (length string) token-length))))
                                   (dotimes (step steps)
                                     (token-reading stream token-length :token))))))))
