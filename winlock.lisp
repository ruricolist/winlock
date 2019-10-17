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

(defctype HANDLE :pointer)
(defctype LPCVOID :pointer)
(defctype LPSTR :pointer)
(defctype LPCWSTR (:string :encoding :utf16-le))
(defctype LPSECURITY_ATTRIBUTES :pointer)
(defctype va_list :pointer)

(defconst GENERIC-READ #x80000000)
(defconst GENERIC-WRITE #x40000000)
(defconst GENERIC-EXECUTE #x20000000)
(defconst GENERIC-ALL #x10000000)

(defconst no-sharing 0)
(defconst FILE-SHARE-DELETE #x00000004)
(defconst FILE-SHARE-READ #x00000001)
(defconst FILE-SHARE-WRITE #x00000002)

(defconst CREATE-ALWAYS 2)
(defconst CREATE-NEW 1)
(defconst OPEN-ALWAYS 4)
(defconst OPEN-EXISTING 3)
(defconst TRUNCATE-EXISTING 5)

(defconst FILE-ATTRIBUTE-NORMAL 128)
(defconst FILE-ATTRIBUTE-TEMPORARY 256)

(defconst FILE-FLAG-POSIX-SEMANTICS #x0100000)
(defconst FILE-FLAG-RANDOM-ACCESS #x10000000)

(def INVALID-HANDLE-VALUE -1)

(defconstructor file-handle
  (file string)
  (handle t))

;;; Returns INVALID-HANDLE-VALUE (-1) on error.
(defcfun ("CreateFileW" %create-file :convention :stdcall) :int
  (lpFileName LPCWSTR)
  (dwDesiredAccess DWORD)
  (dwShareMode DWORD)
  (lpSecurityAttributes LPSECURITY_ATTRIBUTES)
  (dwCreationDisposition DWORD)
  (dwFlagsAndAttributes DWORD)
  (hTemplateFile HANDLE))

(defcfun ("CloseHandle" close-handle :convention :stdcall) :boolean
  (hObject HANDLE))

(defun lockfile (file)
  (string+ (native-namestring file) ".lock"))

(defun lock-file (file)
  (lock-file-1 (lockfile file)))

(defun lock-file-1 (file)
  (let* ((file (native-namestring file))
         (handle (create-file file)))
    (file-handle file handle)))

(defun create-file (file)
  (declare (string file))
  (let ((val
          (with-foreign-string (f file :encoding :utf-16le)
            (%create-file f
                          (logior GENERIC-READ GENERIC-WRITE)
                          no-sharing
                          (null-pointer)
                          OPEN-ALWAYS
                          FILE-ATTRIBUTE-NORMAL
                          (null-pointer)))))
    (if (eql val INVALID-HANDLE-VALUE)
        (error 'winlock-error :code (get-last-error))
        (make-pointer val))))

(defun unlock-file (handle)
  (close-handle (file-handle-handle handle)))

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

(defun lock-handle (handle)
  (let ((overlapped (overlapped)))
    (unwind-protect
         (or (lock-file-ex handle
                           (logior LOCKFILE-EXCLUSIVE-LOCK
                                   LOCKFILE-FAIL-IMMEDIATELY)
                           0
                           MAXDWORD
                           MAXDWORD
                           overlapped)
             (error 'winlock-error :code (get-last-error)))
      (cffi:foreign-free overlapped))))

(defun unlock-handle (handle)
  (let ((overlapped (overlapped)))
    (unwind-protect
         (or (unlock-file-ex handle
                             0
                             MAXDWORD
                             MAXDWORD
                             overlapped)
             (error 'winlock-error :code (get-last-error)))
      (cffi:foreign-free overlapped))))

;;; Lightly adapted from the winhttp library.

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
