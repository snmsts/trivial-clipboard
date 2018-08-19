;;;
;;; Windows support using Win32 API
;;;

(cl:in-package :trivial-clipboard)

(load-foreign-library '(:default "User32"))

;; Flags
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +win32-cf-unicodetext+ 13)
  (defconstant +win32-gmem-moveable+ 2)
  (defconstant +win32-gmem-ddeshare+ 8192))


;; Relevant declarations:
;; BOOL OpenClipboard(HWND hWndNewOwner);
;; BOOL CloseClipboard();
;; HANDLE GetClipboardData(UINT uFormat);
;; HANDLE SetClipboardData(UINT uFormat, HANDLE hMem);
;; BOOL EmptyClipboard(VOID);
;; HGLOBAL GlobalAlloc(UINT uFlags, SIZE_T dwBytes);
;; HGLOBAL GlobalFree(HGLOBAL hMem);
;; LPVOID GlobalLock(HGLOBAL hMem);
;; BOOL GlobalUnlock(HGLOBAL hMem);
;; wchar_t *wcscpy(wchar_t *strDestination, const wchar_t *strSource);
;; size_t wcslen(const wchar_t *str);

(defun get-text-on-win32 ()
  "Get a string from clipboard. Return NIL when clipboard is NULL."
  (let ((*default-foreign-encoding* :utf-16le))
    (unwind-protect
         (if (foreign-funcall "OpenClipboard"
                              :pointer (null-pointer)
                              :boolean)
             (let ((hmem (foreign-funcall "GetClipboardData"
                                          :uint +win32-cf-unicodetext+
                                          :pointer)))
               (if (null-pointer-p hmem)
                   nil ; No data in clipboard.
                   (unwind-protect
                        (foreign-funcall "GlobalLock"
                                         :pointer hmem
                                         :string)
                     (foreign-funcall "GlobalUnlock" :pointer hmem))))
             (error "OpenClipboard failed."))
      (foreign-funcall "CloseClipboard" :boolean))))

(defun set-text-on-win32 (string)
  "Set a string to clipboard."
  (let ((*default-foreign-encoding* :utf-16le))
    (with-foreign-string (cstring string)
      (let ((hmem
              (foreign-funcall "GlobalAlloc"
                               :uint #.(logior +win32-gmem-ddeshare+
                                               +win32-gmem-moveable+)
                               :uint (* 2 (1+ (foreign-funcall "wcslen"
                                                               :pointer cstring
                                                               :uint)))
                               :pointer)))
        (when (null-pointer-p hmem)
          (error "GlobalAlloc failed."))
        (handler-bind
            ((error (lambda (c)
                      ;; HMEM must be freed if and only if it was not
                      ;; successfully set to the clipboard.
                      (declare (ignore c))
                      (foreign-funcall "GlobalFree" :pointer hmem :pointer))))
          (unwind-protect
               (foreign-funcall "wcscpy"
                                :pointer (foreign-funcall "GlobalLock"
                                                          :pointer hmem
                                                          :pointer)
                                :pointer cstring
                                :pointer)
            (foreign-funcall "GlobalUnlock" :pointer hmem))
          (unwind-protect
               (progn
                 (or (foreign-funcall "OpenClipboard"
                                      :pointer (null-pointer)
                                      :boolean)
                     (error "OpenClipboard failed."))
                 (or (foreign-funcall "EmptyClipboard" :boolean)
                     (error "EmptyClipboard failed."))
                 (or (not (null-pointer-p
                           (foreign-funcall "SetClipboardData"
                                            :uint +win32-cf-unicodetext+
                                            :pointer hmem
                                            :pointer)))
                     (error "SetClipboardData failed.")))
            (foreign-funcall "CloseClipboard" :boolean)))))))
