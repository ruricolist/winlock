Winlock is a library that allows locking a file, using system calls, on Windows.

``` lisp
(winlock:with-locked-file (#p"~/some/file")
  (do-something))
```

The file can be opened for read, write, or both, and be shared (`:shared t`) or exclusive (`:shared nil`).

``` lisp
(with-locked-file (file :direction :input :shared t))
```

Defaults: `:direction` defaults to `:input`; `:shared` defaults to `t` if `:direction` is `:input`, and `nil` otherwise. The direction can be any of `:input`, `:output`, or `:io`.

Note that the file is not opened by locking it.

Besides `with-locked-file`, there are also unbalanced `lock-file` and `unlock-handle` functions. (There is no `unlock-file`; you have to save the handle returned by lock-file to pass to `unlock-handle`.)

Caveat: the file that is locked is not actually the file itself, but another file with a `.lock` extension. E.g. if you lock `file.txt`, the actual lock is taken on `file.txt.lock`. This is because a file that is locked on Windows is accessible *only* through the handle used to lock it – even the same program cannot open the file with a different handle. You can circumvent this by calling `lock-file` with `:direct t`; but writing a stream implementation that works on the handle returned is left an exercise for the reader.

Some code in this library (for error reporting) is adapted from [winhttp][].

This is intended as a building block for a portable file-locking library.

[winhttp]: https://github.com/fjames86/winhttp
