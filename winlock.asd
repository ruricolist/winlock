;;;; winlock.asd

(defsystem "winlock"
  :description "File locking using the Windows API."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :in-order-to ((test-op (test-op "winlock/test")))
  :depends-on ("serapeum" "cffi" "named-readtables")
  :serial t
  :components ((:file "package")
               (:file "readtable")
               (:file "winlock")))

(defsystem "winlock/test"
  :description "Test suite for Winlock."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on ("winlock" "fiveam")
  :perform (test-op (o c) (symbol-call :winlock/test :run-tests))
  :serial t
  :components ((:file "test")))
