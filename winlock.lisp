(in-package :winlock)
(in-readtable case-inverting-readtable)

(load-foreign-library "kernel32.dll")

(deftype direction ()
  '(member :input :output :io))

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
(defconst FILE-FLAG-DELETE-ON-CLOSE #x04000000)

(defconst FILE-FLAG-POSIX-SEMANTICS #x0100000)
(defconst FILE-FLAG-RANDOM-ACCESS #x10000000)

(defconst INVALID-HANDLE-VALUE -1)

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

(defun lock-file (file &key
                         (direction :input)
                         (shared (eql direction :input))
                         direct)
  (lret* ((file
           (assure string
             (if direct
                 (native-namestring file)
                 (lockfile file))))
          (access
           (ecase-of direction direction
             (:input GENERIC-READ)
             (:output GENERIC-WRITE)
             (:io (logior GENERIC-READ GENERIC-WRITE))))
          (share-mode
           (if shared
               (logior
                ;; This is required because of FILE-FLAG-DELETE-ON-CLOSE.
                (if direct 0 FILE-SHARE-DELETE)
                (ecase-of direction direction
                  (:input FILE-SHARE-READ)
                  (:output FILE-SHARE-WRITE)
                  (:io (logior FILE-SHARE-READ FILE-SHARE-WRITE))))
               no-sharing))
          (disposition
           (if direct
               OPEN-EXISTING
               OPEN-ALWAYS))
          (attrs
           (logior FILE-FLAG-POSIX-SEMANTICS
                   (if direct 0
                       (logior FILE-ATTRIBUTE-TEMPORARY
                               FILE-FLAG-DELETE-ON-CLOSE))))
          (val
           (with-foreign-string (f file :encoding :utf-16le)
             (%create-file f
                           access
                           share-mode
                           (null-pointer)
                           disposition
                           attrs
                           (null-pointer))))
          (handle
           (if (eql val INVALID-HANDLE-VALUE)
               (error 'winlock-error :code (get-last-error))
               (make-pointer val)))
          (file-handle (file-handle file handle)))
    (tg:finalize file-handle
                 (lambda ()
                   (ignore-errors
                    (close-handle handle))))))

(defun unlock-handle (handle)
  (close-handle (file-handle-handle handle)))

(defcfun ("GetLastError" get-last-error :convention :stdcall) DWORD)

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

(defmacro with-locked-file ((file &rest args &key &allow-other-keys)
                            &body body)
  (with-thunk (body)
    `(call/locked-file ,body ,file ,@args)))

(defun call/locked-file (fn file &rest args &key &allow-other-keys)
  (let ((handle (apply #'lock-file file args)))
    (unwind-protect
         (funcall fn)
      (unlock-handle handle))))
