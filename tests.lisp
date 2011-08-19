;;;; By Nikodemus Siivola <nikodemus@random-state.net>, 2011
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation files
;;;; (the "Software"), to deal in the Software without restriction,
;;;; including without limitation the rights to use, copy, modify, merge,
;;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;;; and to permit persons to whom the Software is furnished to do so,
;;;; subject to the following conditions:
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(defpackage :sb-vector-io-tests
  (:use :cl :sb-vector-io)
  (:export "RUN-TESTS"))

(in-package :sb-vector-io-tests)

(defmacro with-test-directory ((dir) &body body)
  `(let (,dir)
     (unwind-protect
          (progn
            (setf ,dir (truename (sb-posix:mkdtemp "sb-vector-io-XXXXXX")))
            (let ((*default-pathname-defaults* ,dir))
              ,@body))
       (ignore-errors
        (sb-ext:delete-directory ,dir :recursive t)))))

(defun run-tests ()
  (sb-rt:do-tests))

(sb-rt:deftest simple-ub8.1
    (let ((b (make-array 3 :element-type '(unsigned-byte 8)
                           :initial-contents (list 1 2 3))))
      
      (with-test-directory (dir)
        (let ((name "simple-ub.1"))
          (with-open-file (f name :direction :output
                                  :element-type '(unsigned-byte 8))
            (assert (= 3 (write-vector-data b f))))
          (fill b 0)
          (with-open-file (f name :direction :input
                                  :element-type '(unsigned-byte 8))
            (assert (= 3 (read-vector-data b f))))))
      (values (aref b 0)
              (aref b 1)
              (aref b 2)))
  1
  2
  3)

(sb-rt:deftest pipe-timeout.double-float.1
    (multiple-value-bind (in out) (sb-posix:pipe)
      (sb-posix:fcntl in sb-posix:f-setfl sb-posix:o-nonblock)
      (sb-posix:fcntl out sb-posix:f-setfl sb-posix:o-nonblock)
      (let ((sout (sb-sys:make-fd-stream out :output t
                                          :element-type '(unsigned-byte 8)
                                          :timeout 0.1))
            (b (make-array 12 :element-type 'double-float
                              :initial-contents (list 1d0 2d0 3d0 4d0
                                                      5d0 6d0 7d0 8d0
                                                      9d0 10d0 11d0 12d0)))
            (n 0))
        (assert (eq :timeout
                    (handler-case
                        (loop
                          ;; Eventually the pipe fills up and this times out
                          (write-vector-data b sout)
                          (incf n))
                      (sb-sys:io-timeout ()
                        :timeout))))
        (let ((sin (sb-sys:make-fd-stream in :input t
                                             :element-type '(unsigned-byte 8)
                                             :timeout 0.1))
              (x (make-array 12 :element-type 'double-float)))
          (assert (eq :timeout
                      (handler-case
                          (loop
                            (fill x 0d0)
                            (multiple-value-bind (c r)
                                (read-vector-data x sin)
                              (assert (equalp x b))
                              (assert (= 12 c))
                              (assert (not r))
                              (decf n)))
                        (sb-sys:io-timeout ()
                          :timeout))))
          (assert (zerop n)))
        t))
  t)
