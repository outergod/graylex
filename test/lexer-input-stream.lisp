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

(deftest lexer-unreading (stream expected-position expected-double-buffer)
  (is (= expected-position (lexer-non-stream-position stream)))
  (is (string= expected-double-buffer (lexer-double-buffer stream))))

(deftest lexer-simple-flushing ()
  (simple-flushing 'lexer-input-stream t))

(deftest lexer-simple-reading ()
  (simple-reading 'lexer-input-stream t))

(deftest lexer-sequence-reading ()
  (simple-sequence-reading 'lexer-input-stream t))
