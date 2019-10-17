(uiop:define-package :winlock
  (:use :cl :named-readtables :cffi)
  (:mix :uiop :alexandria :serapeum)
  (:export
   #:lock-file
   #:unlock-handle
   #:with-locked-file))
