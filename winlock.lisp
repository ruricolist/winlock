(in-package :winlock)
(in-readtable case-inverting-readtable)

(load-foreign-library "kernel32.dll")

(defconst MAXDWORD #xffffffff)

(defconst LOCKFILE-EXCLUSIVE-LOCK #x02
  "The function requests an exclusive lock. Otherwise, it requests a shared lock.")

(defconst LOCKFILE-FAIL-IMMEDIATELY #x01
  "The function returns immediately if it is unable to acquire the requested lock. Otherwise, it waits.")

(defconst FORMAT-MESSAGE-FROM-SYSTEM #x00001000
  "The function should search the system message-table resource(s) for the requested message.")

(defctype DWORD :unsigned-int)

;; (defctype HANDLE (:pointer :void))
(defctype HANDLE :pointer)
(defctype LPCVOID :pointer)
(defctype LPSTR :pointer)
(defctype va_list :pointer)

(defcstruct _overlapped
  (Internal :unsigned-long)
  (InternalHigh :unsigned-long)
  (hEvent :unsigned-long))

(defun overlapped ()
  (lret ((o (foreign-alloc '(:struct _overlapped))))
    (with-foreign-slots ((Internal InternalHigh hEvent) o (:struct _overlapped))
      (setf Internal 0 InternalHigh 0 hEvent 0))))

(defctype LPOVERLAPPED (:pointer (:struct _overlapped)))

(defcfun ("LockFileEx" lock-file-ex :convention :stdcall) :boolean
  (hFile HANDLE)
  (dwFlags DWORD)
  (dwReserved DWORD)
  (nNumberOfBytesToLockLow DWORD)
  (nNumberOfBytesToLockHigh DWORD)
  (lpOverlapped LPOVERLAPPED))

(defcfun ("UnlockFileEx" unlock-file-ex :convention :stdcall) :boolean
  (hFile HANDLE)
  (dwReserved DWORD)
  (nNumberOfBytesToUnlockLow DWORD)
  (nNumberOfBytesToUnlockHigh DWORD)
  (lpOverlapped LPOVERLAPPED))

(defcfun ("GetLastError" get-last-error :convention :stdcall) DWORD)

(defun lock-stream-file (stream direction)
  (lock-handle (ccl::stream-device stream direction)))

(defun lock-handle (handle)
  (let ((overlapped (overlapped)))
    (unwind-protect
         (or (lock-file-ex (make-pointer handle)
                           (logior LOCKFILE-EXCLUSIVE-LOCK
                                   LOCKFILE-FAIL-IMMEDIATELY)
                           0
                           MAXDWORD
                           MAXDWORD
                           overlapped)
             (error 'winlock-error :code (get-last-error)))
      (cffi:foreign-free overlapped))))

(defun unlock-stream-file (stream direction)
  (unlock-handle (ccl::stream-device stream direction)))

(defun unlock-handle (handle)
  (let ((overlapped (overlapped)))
    (unwind-protect
         (or (unlock-file-ex (make-pointer handle)
                             0
                             MAXDWORD
                             MAXDWORD
                             overlapped)
             (error 'winlock-error :code (get-last-error)))
      (cffi:foreign-free overlapped))))

(defcfun (%format-message "FormatMessageA" :convention :stdcall) DWORD
  (dwFlags DWORD)
  (source LPCVOID)
  (dwMessageId DWORD)
  (dwLanguageId DWORD)
  (lpBuffer LPSTR)
  (nSize DWORD)
  (Arguments va_list))

(defun format-message (code)
  "Use FormatMessage to convert the error code into a system-defined string."
  (with-foreign-object (buffer :char 1024)
    (let ((n (%format-message (logior FORMAT-MESSAGE-FROM-SYSTEM)
                              (null-pointer)
                              code
                              0
                              buffer
                              1024
                              (null-pointer))))
      (if (zerop n)
          (let ((ec (get-last-error)))
            (format nil "Failed to format message ~A: ~A"
                    ec
                    (format-message ec)))
          (foreign-string-to-lisp buffer :count (- n 2))))))

(defcondition winlock-error (error)
  ((code :initarg :code))
  (:report (lambda (c s)
             (with-slots (code) c
               (format s "~a (#~a)" (format-message code) code)))))
