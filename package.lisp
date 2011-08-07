;;;; By Nikodemus Siivola <nikodemus@random-state.net>, 2008, 2011
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

(in-package :cl-user)

(defpackage :sb-vector-io
  (:use :cl)
  (:export "WRITE-VECTOR-DATA" "READ-VECTOR-DATA"))

;;; KLUDGE: trick to deprecate this name
(defpackage :vector-io
  (:use :cl)
  (:export "WRITE-VECTOR-DATA" "READ-VECTOR-DATA"))

(in-package :vector-io)

(defvar *notified* nil)

(defgeneric write-vector-data (vector stream &key start end)
  (:method (vector stream &key (start 0) end)
    (unless *notified*
      (cerror
       "Continue, redirecting to SB-VECTOR-IO. This error will not repeat."
       "Package VECTOR-IO is deprecated, use SB-VECTOR-IO instead.")
      (setf *notified* t))
    (sb-vector-io:write-vector-data vector stream :start start :end end)))

(define-compiler-macro write-vector-data (&whole form &rest args)
  (warn "Package VECTOR-IO is deprecated, use SB-VECTOR-IO instead.")
  form)

(defgeneric read-vector-data (vector stream &key start end)
  (:method (vector stream &key (start 0) end)
    (unless *notified*
      (cerror
       "Continue, redirecting to SB-VECTOR-IO. This error will not repeat."
       "Package VECTOR-IO is deprecated, use SB-VECTOR-IO instead.")
      (setf *notified* t))
    (sb-vector-io:read-vector-data vector stream :start start :end end)))

(define-compiler-macro read-vector-data (&whole form &rest args)
  (warn "Package VECTOR-IO is deprecated, use SB-VECTOR-IO instead.")
  form)
