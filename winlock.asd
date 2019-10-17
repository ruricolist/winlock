;;;; winlock.asd

(defsystem "winlock"
  :description "File locking using the Windows API."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on ("serapeum" "cffi" "named-readtables")
  :serial t
  :components ((:file "package")
               (:file "readtable")
               (:file "winlock")))
