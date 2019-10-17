(defsystem "winlock"
  :depends-on ("serapeum" "cffi" "named-readtables")
  :serial t
  :components ((:file "package")
               (:file "readtable")
               (:file "winlock")))
