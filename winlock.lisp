(in-package :winlock)
(in-readtable case-inverting-readtable)

(load-foreign-library "user32.dll")
(load-foreign-library "kernel32.dll")
(load-foreign-library "msvcrt.dll")

(defconst MAXDWORD #xffffffff)

(defconst LOCKFILE-EXCLUSIVE-LOCK #x02
  "The function requests an exclusive lock. Otherwise, it requests a shared lock.")
(defconst LOCKFILE-FAIL-IMMEDIATELY #x01
  "The function returns immediately if it is unable to acquire the requested lock. Otherwise, it waits.")

(defctype DWORD :unsigned-int)

;; (defctype HANDLE (:pointer :void))
(defctype HANDLE :pointer)

(defcstruct _overlapped
  (Internal :unsigned-long)
  (InternalHigh :unsigned-long)
  (hEvent :unsigned-long))

(defun overlapped ()
  (lret ((o (foreign-alloc '(:struct _overlapped))))
    (with-foreign-slots ((Internal InternalHigh hEvent) o (:struct _overlapped))
      (setf Internal 0 InternalHigh 0 hEvent 0))))

(defctype LPOVERLAPPED (:pointer (:struct _overlapped)))

(defcfun ("LockFileEx" lock-file-ex) :boolean
  (hFile HANDLE)
  (dwFlags DWORD)
  (dwReserved DWORD)
  (nNumberOfBytesToLockLow DWORD)
  (nNumberOfBytesToLockHigh DWORD)
  (lpOverlapped LPOVERLAPPED))

(defcfun ("UnlockFileEx" unlock-file-ex) :boolean
  (hFile HANDLE)
  (dwReserved DWORD)
  (nNumberOfBytesToUnlockLow DWORD)
  (nNumberOfBytesToUnlockHigh DWORD)
  (lpOverlapped LPOVERLAPPED))

(defcfun ("GetLastError" get-last-error) DWORD)

;;; TODO GetLastError
;;; https://docs.microsoft.com/en-us/windows/win32/debug/system-error-codes--0-499-

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
             (get-last-error))
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
             (get-last-error))
      (cffi:foreign-free overlapped))))
