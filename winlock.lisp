(in-package :winlock)
(in-readtable case-inverting-readtable)

(load-foreign-library "user32.dll")
(load-foreign-library "kernel32.dll")
(load-foreign-library "msvcrt.dll")

;;; lock_ex = LOCKFILE_EXCLUSIVE_LOCK
;;; Lock_SH = 0
;;; lock_nb = LOCKfile_fail_immediately

(defconst MAXDWORD #xffffffff)

(defconst LOCKFILE-EXCLUSIVE-LOCK #x02
  "The function requests an exclusive lock. Otherwise, it requests a shared lock.")
(defconst LOCKFILE-FAIL-IMMEDIATELY #x01
  "The function returns immediately if it is unable to acquire the requested lock. Otherwise, it waits.")

;;; TODO only unlock if the process id is the same?

(defctype DWORD :unsigned-int)

;; (defctype HANDLE (:pointer :void))
(defctype HANDLE :unsigned-int)

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

;;; TODO GetLastError

(defun lock-stream-file (stream direction)
  (let ((device (ccl::stream-device stream direction))
        (overlapped (overlapped)))
    (unwind-protect
         (lock-file-ex device
                       (logior LOCKFILE-EXCLUSIVE-LOCK
                               LOCKFILE-FAIL-IMMEDIATELY)
                       0
                       MAXDWORD
                       MAXDWORD
                       overlapped)
      (cffi:foreign-free overlapped))))

(defun unlock-stream-file (stream direction)
  (let ((device (ccl::stream-device stream direction))
        (overlapped (overlapped)))
    (unwind-protect
         (unlock-file-ex device
                         0
                         MAXDWORD
                         MAXDWORD
                         overlapped)
      (cffi:foreign-free overlapped))))

;;; ! lockfileex(file, dwFlags, 0, 1, 0, &ov))
;;;
