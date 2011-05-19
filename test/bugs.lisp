;;;; graylex - bugs
;;;; Copyright (C) 2011  Alexander Kahl <e-user@fsfe.org>
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

(in-suite lexer)
(defsuite bugs)
(in-suite bugs)

;;; Regression test suite

(deftest github-issue-1 ()
  "Test for Github issue 1 regression.

See https://github.com/e-user/graylex/issues/1"
  (mapc #'(lambda (rule)
            (with-buffer-input-from-string (stream 'lexer-input-stream 3 "abcdef"
                                                   :rules `(,rule
                                                            ("." . :fail)))
              (do ((class (stream-read-token stream)
                          (stream-read-token stream)))
                  ((null class))
                (is (eq class :win)))))
        '(("[abcdef]{2}" . :win)
          ("[abcdef]{1,2}" . :win))))

(deftest github-issue-1-actual ()
  "Test for Github issue 1 regression using actual test data from the user.

See https://github.com/e-user/graylex/issues/1"
  (mapc #'(lambda (file)
            (with-open-file (stream file)
              (let ((lexer (make-instance 'lexer-input-stream
                                          :stream stream
                                          :rules '(("\\d\\. \\S*.*?\\n|$" . :list)
                                                   (".*?\\S.*?(\\n|$)" . :nonempty)
                                                   ("[\\t\\r\\f ]*(\\n|$)" . :empty)))))
                (mapc #'(lambda (class)
                          (is (eq class (stream-read-token lexer))))
                      '(:nonempty :list :list :list)))))
        (cl-fad:list-directory (asdf:system-relative-pathname (intern (package-name *package*)) "test/fixtures/github-issue-1/"))))
