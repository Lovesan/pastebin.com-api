(in-package :cl-user)

(defpackage #:pastebin.com-api
  (:use :cl #:drakma #:puri)
  (:export #:pastebin-post
           #:list-available-file-formats
           #:pastebin-api-error
           #:pastebin-api-error-message
           #:pastebin-api-invalid-file-format
           #:pastebin-api-invalid-file-format-format
           #:pastebin-api-invalid-request))
