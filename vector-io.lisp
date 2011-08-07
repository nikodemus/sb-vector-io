;;;; Writing and reading raw vector data in SBCL
;;;;
;;;; Works with vectors -- both non-simple and simple, if the element type of
;;;; the vector is such that it conforms to "normal" way of representing such
;;;; data on the platform. Endianness is unspecified, floating points use the
;;;; IEEE format.
;;;;
;;;; By Nikodemus Siivola <nikodemus@random-state.net>, 2008.
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

#-sbcl (error "Unsupported implementation.")

(in-package :vector-io)

(defgeneric stream-fd (stream direction)
  (:documentation
   "Returns the underlying file-descriptor of STREAM in DIRECTION. DIRECTION
must be either :INPUT or :OUTPUT.")

  (:method ((stream sb-sys:fd-stream) direction)
    (declare (ignore direction))
    (sb-sys:fd-stream-fd stream))

  (:method ((stream synonym-stream) direction)
    (stream-fd (symbol-value (synonym-stream-symbol stream)) direction))

  (:method ((stream two-way-stream) direction)
    (ecase direction
      (:input
       (stream-fd
        (two-way-stream-input-stream stream) direction))
      (:output
       (stream-fd
        (two-way-stream-output-stream stream) direction)))))

(defgeneric stream-timeout (stream direction)
  (:documentation
   "Returns the underlying timeout of STREAM in DIRECTION. DIRECTION
must be either :INPUT or :OUTPUT.")

  (:method ((stream sb-sys:fd-stream) direction)
    (declare (ignore direction))
    (sb-impl::fd-stream-timeout stream))

  (:method ((stream synonym-stream) direction)
    (stream-timeout (symbol-value (synonym-stream-symbol stream)) direction))

  (:method ((stream two-way-stream) direction)
    (ecase direction
      (:input
       (stream-timeout
        (two-way-stream-input-stream stream) direction))
      (:output
       (stream-timeout
        (two-way-stream-output-stream stream) direction)))))

(defun vector-element-bytes (vector)
  (let ((aet (array-element-type vector)))
    (etypecase aet
      ((member double-float) 8)
      ((member single-float) 4)
      ((member fixnum) #.sb-vm:n-word-bytes)
      (cons
       (let ((type (first aet))
             (bits (second aet)))
         (check-type type (member unsigned-byte signed-byte))
         (ceiling (the fixnum bits) 8))))))

(defun check-stream-element-type (stream context)
  (or (and (sb-impl::fd-stream-p stream)
           (sb-impl::fd-stream-bivalent-p stream))
      (let ((set (stream-element-type stream)))
        (unless (subtypep set '(or signed-byte unsigned-byte))
          (error "Invalid stream element type for ~S: ~S" context set)))))

;;; WRITE-VECTOR-DATA

(defgeneric write-vector-data (vector stream &key start end)
  (:documentation
   "Writes elements of the subsequence of VECTOR bounded by START and
END to STREAM.

VECTOR must have ARRAY-ELEMENT-TYPE of FIXNUM, DOUBLE-FLOAT,
SINGLE-FLOAT, \(UNSIGNED-BYTE X), or \(SIGNED-BYTE X). Floating point
values are written using the IEEE formats, and SIGNED-BYTE and
UNSIGNED-BYTE values are written as raw bytes using the native
endianness of the host platform. FIXNUMs are written in the
implementation internal representation.

STREAM must be a binary output stream.

Returns the number of elements written."))

(defmethod write-vector-data ((vector vector) (stream sb-kernel:ansi-stream) &key (start 0) end)
  (check-stream-element-type stream 'write-vector-data)
  ;; KLUDGE: SBCL normally buffers all output (to be fixed later), and
  ;; since the raison d'etre for VECTOR-IO is efficient IO for large
  ;; vectors of simple data, we are going to bypass buffering -- so
  ;; the first thing to do is to flush any pending output.
  (finish-output stream)
  (sb-kernel:with-array-data ((data vector)
                              (data-start start)
                              (data-end end)
                              :check-fill-pointer t)
    (let* ((elt-size (vector-element-bytes data))
           (elt-count (- data-end data-start))
           (byte-count (* elt-size elt-count))
           (byte-start (* elt-size data-start))
           (fd (stream-fd stream :output)))
      (tagbody
       :write
         (multiple-value-bind (wrote errno)
             (sb-unix:unix-write fd data byte-start byte-count)
           (cond ((eql wrote byte-count)
                  ;; Done!
                  (return-from write-vector-data elt-count))
                 (wrote
                  ;; Partial write
                  (decf byte-count wrote)
                  (incf byte-start wrote)
                  (go :write))
                 ;; We don't have serve-event on Windows, so no way to deal
                 ;; with incomplete writes, but as long as the underlying
                 ;; handle is not asynchronous, unix-write should always
                 ;; complete -- we hope!
                 #-win32
                 ((eql errno sb-unix:ewouldblock)
                  ;; Blocking, must wait or serve events.
                  (if (sb-impl::fd-stream-serve-events stream)
                      (wait-for-vector-write stream fd data
                                             data-start byte-count)
                      (if (sb-sys:wait-until-fd-usable
                           fd :output (stream-timeout stream) nil)
                          (go :write)
                          (sb-impl::signal-timeout
                           'sb-impl::io-timeout
                           :stream stream
                           :direction :output
                           :seconds (stream-timeout stream))))
                  (return-from write-vector-data elt-count))
                 (t
                  (sb-impl::simple-stream-perror
                   "Could't write to ~S" stream errno))))))))

#-win32
(defun wait-for-vector-write (stream fd data start bytes)
  ;; FIXME: this bypasses the normal FD-STREAM output queue, but that
  ;; should be fine assuming that no other fd-handler firing causes
  ;; writes to the same file descriptor -- and since the stream should
  ;; be flushed, this is the case under normal circumstances: the only
  ;; exception is user added fd-handlers triggering complex activity.
  ;;
  ;; When this code is integrated to SBCL, the code path that now
  ;; starts with BUFFER-OUTPUT should first try to write directly
  ;; (assuming there is no queued output), and if it fails, be able to
  ;; use the object being written directly instead of copying to a
  ;; buffer. (Standard output functions which may return after output
  ;; has been buffered must copy their output, but we can specify
  ;; unspecified consequences for WRITE-VECTOR-DATA if a part of it
  ;; that has been output is modified before the stream it was written
  ;; to is flushed.)
  (let (handler)
    (setf handler
          (lambda (fd)
            (multiple-value-bind (wrote errno)
                (sb-unix:unix-write fd data start bytes)
              (cond ((eql bytes wrote)
                     ;; Done, remove handler
                     (when handler
                       (sb-sys:remove-fd-handler handler))
                     (setf handler nil))
                    (wrote
                     ;; Another partial write
                     (decf bytes wrote)
                     (incf start wrote))
                    ((eql errno sb-unix:ewouldblock)
                     ;; Blocks again, keep waiting
                     )
                    (t
                     (let (continue)
                       (unwind-protect
                            (progn
                              (with-simple-restart (continue
                                                    "Keep trying to write.")
                                (sb-impl::simple-stream-perror
                                 "Could't write to ~S" stream errno))
                              (setf continue t))
                         (unless continue
                           (when handler
                             (sb-sys:remove-fd-handler handler))))))))))
    (sb-sys:add-fd-handler fd :output handler)
    (loop while handler do (sb-sys:serve-event))))

;;;; READ-VECTOR-DATA

(defgeneric read-vector-data (vector stream &key start end)
  (:documentation
   "Destructively modifies VECTOR by replacing elements of the subsequence
bounded by START and AND with raw data from STREAM.

VECTOR must have ARRAY-ELEMENT-TYPE of DOUBLE-FLOAT, SINGLE-FLOAT,
\(UNSIGNED-BYTE X), or \(SIGNED-BYTE X). Floating point values are
read using the IEEE formats, and SIGNED-BYTE and UNSIGNED-BYTE values
are read as raw bytes using the native endianness of the host
platform. FIXNUMs are read in the implementation internal
representation.

Primary return value is the number of elements read.

If element type of VECTOR is other than \(UNSIGNED-BYTE 8) or \(SIGNED-BYTE
8), and END-OF-FILE occurs before all the requested elements have been read,
the last element read may be incomplete. In such a case the secondary return
value is the number of bytes missing from the last element -- NIL if the last
element is complete."))

(defmethod read-vector-data ((vector vector) (stream sb-kernel:ansi-stream) &key (start 0) end)
  (check-stream-element-type stream 'read-vector)
  (sb-kernel:with-array-data ((data vector)
                              (data-start start)
                              (data-end end)
                              :check-fill-pointer t)
    (let* ((elt-size (vector-element-bytes data))
           (elt-count (- data-end data-start))
           (byte-count (* elt-size elt-count))
           (byte-start (* elt-size data-start))
           ;; binary stream: no problems with unread characters
           (read (sb-impl::ansi-stream-read-n-bytes stream data byte-start byte-count nil)))
      (multiple-value-bind (n rem) (truncate read elt-size)
        (values n
                (if (zerop rem)
                    nil
                    (- elt-size rem)))))))
