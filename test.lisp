(defpackage :winlock/test
  (:use :cl :alexandria :serapeum :winlock :fiveam)
  (:import-from #:uiop
    #:native-namestring
    #:with-temporary-file
    #:file-exists-p
    #:parse-native-namestring)
  (:documentation "Basic test suite for file locking.")
  (:export
   #:run-tests))
(in-package :winlock/test)

(def-suite winlock)
(in-suite winlock)

(defun run-tests ()
  (run! 'winlock))

(test lockfile
  (with-temporary-file (:pathname p)
    (let ((lockfile
            (parse-native-namestring
             (winlock::lockfile p))))
      (is-true (file-exists-p p))
      (is-false (file-exists-p lockfile))
      (with-locked-file (p)
        (is-true (file-exists-p lockfile)))
      (is-false (file-exists-p lockfile))
      (is-true (file-exists-p p)))))

(test no-lockfile
  (with-temporary-file (:pathname p)
    (write-string-into-file "hello" p :if-exists :overwrite)
    (let ((lockfile (parse-native-namestring
                     (winlock::lockfile p))))
      (is-true (file-exists-p p))
      (is-false (file-exists-p lockfile))
      (with-locked-file (p :direct t)
        (is-false (file-exists-p lockfile)))
      (with-locked-file (p :direct t :shared nil)
        (is-false (file-exists-p lockfile))
        (signals error
          (read-file-into-string p)))
      (is-false (file-exists-p lockfile))
      (is-true (file-exists-p p)))))

(test unshared
  (with-temporary-file (:pathname p)
    (with-locked-file (p :shared nil)
      (signals error
        (with-locked-file (p :shared nil))))))

(test shared
  (finishes
    (with-temporary-file (:pathname p)
      (with-locked-file (p :shared t)
        (with-locked-file (p :shared t))))))

(defun cat (file)
  (uiop:run-program
   (fmt "type ~a"
        (substitute #\\ #\/
                    (native-namestring file)))))

(test other-process-unshared
  (with-temporary-file (:pathname p)
    (finishes (cat p))
    (with-locked-file (p :shared nil :direct t)
      (signals error (cat p)))))

(test other-process-shared
  (with-temporary-file (:pathname p)
    (with-locked-file (p :shared t :direct t)
      (finishes (cat p)))))
