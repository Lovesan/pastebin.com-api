(in-package #:pastebin.com-api)

(define-condition pastebin-api-error (error)
  ((message :initarg :message
            :accessor pastebin-api-error-message))
  (:report (lambda (c s)
             (format s "~a" (slot-value c 'message))))
  (:default-initargs
    :message "An unidentified error occured while interacting with pastebin.com"))

(define-condition pastebin-api-invalid-file-format (pastebin-api-error)
  ((format :initarg :format
           :accessor pastebin-api-invalid-file-format-format))
  (:report (lambda (c s)
             (format s "~a~%File format was: ~a"
                     (slot-value c 'message)
                     (slot-value c 'format)))))

(define-condition pastebin-api-invalid-request (pastebin-api-error) ())

(defun lookup-format (format extension)
  (declare (type (or (eql t) null string) format)
           (type (or null string) extension))
  (cond
    ((null format) "text")
    ((eq format T) (if (null extension)
                     "text"
                     (loop :for (format name . exts)
                           :in +pastebin-file-formats+
                           :when (find extension exts :test #'string-equal)
                           :do (return format)
                           :finally (return "text"))))    
    (T format)))

#+win32
(defun open-with-file-protocol-handler (path)
  (declare (type string path))
  (not (member
         (asdf:run-shell-command
           (concatenate
             'string
             "rundll32 url.dll, FileProtocolHandler "
             path))
         '(0 nil)
         :test #'eql)))

(defun pastebin-post (content &key (name nil)
                                   (email nil)
                                   (private nil)
                                   (subdomain nil)
                                   (expire-date :never)
                                   (format T)
                                   (external-format :default)
                                   #+win32
                                   (open-with-default-browser nil))
"Sends POST request to http://pastebin.com attempting to paste
code designated by CONTENT. On success, returns PURI:URI structure that
designates address of the paste committed.

CONTENT may be a string, representing code itself or a pathname,
representing file, contents of which should be added to pastebin.

:NAME, :EMAIL and :SUBDOMAIN should be a strings, designating name or title
of your paste, email, for sending conformation email with paste link, and
subdomain of pastebin.com, to which code should be pasted, correspondingly.
This values can also be NILs, in which case they will be interpreted as
empty strings

:PRIVATE designates privacy level of the paste. When this parameter is NIL,
you paste will not be added to 'recent posts' etc.

:EXPIRE-DATE designates expiration date of the paste.
It may be :NEVER (the default case), :TEN-MINUTES, :HOUR, :DAY, :MONTH, 
or NIL(which is the same as :NEVER)

:FORMAT argument describes pastebin syntax highlighting format. It can be NIL,
which means no highlighting. It also can be T, which only make sense if
CONTENT is a pathname - in this case format will be guessed by a file extension
of that pathname. And finally, it can also be a string designator which
designate format name. List of available format names can be
obtained by function LIST-AVAILABLE-FILE-FORMATS.

:EXTERNAL-FORMAT is only used when CONTENT is a pathname, and is passed to
CL:OPEN function. It indicates implementation dependent external format,
in which that file should be read.

Finally, on Windows, there's another available parameter:
:OPEN-WITH-DEFAULT-BROWSER
Unless this parameter is NIL, system will try to open recieved pastebin.com
link with default browser."
  (declare (type (or pathname string) content)
           (type (or null string) name email subdomain)
           (type (member :never :ten-minutes :hour :day :month nil)
                 expire-date)
           (type (or (eql T) null string character symbol) format))
  (when (and (pathnamep content)
             (null (pathname-name content)))
    (error "In case of pathname, CONTENT must be a file spec"))
  (let* ((private (if private "1" "0"))
         (expire-date (case expire-date
                        (:never "N")
                        (:ten-minutes "10M")
                        (:hour "1H")
                        (:day "1D")
                        (:month "1M")))
         (format (lookup-format
                   (case format
                     ((T NIL) format)
                     (otherwise (string-downcase (string format))))
                   (if (pathnamep content) (pathname-type content) nil)))
         (content (if (pathnamep content)                     
                    (with-open-file
                        (in content :external-format external-format)
                      (let ((buffer (make-array
                                      (file-length in)
                                      :element-type 'character
                                      :adjustable t)))
                        (adjust-array buffer (read-sequence buffer in))
                        buffer))
                    content)))
    (multiple-value-bind
        (response status-code headers uri stream must-close reason-phrase)
        (http-request
          "http://pastebin.com/api_public.php"
          :method :post
          :parameters (list (cons "paste_private" private)
                            (cons "paste_expire_date" expire-date)
                            (cons "paste_name" name)
                            (cons "paste_email" email)
                            (cons "paste_subdomain" subdomain)
                            (cons "paste_format" format)
                            (cons "paste_code" content)))
      (declare (ignore headers uri stream must-close))
      (if (= status-code 200)
        (cond
          ((string= response
                    "ERROR: Invalid file format")
           (error 'pastebin-api-invalid-file-format
                  :message response
                  :format format))
          ((string= response
                    "ERROR: Invalid POST request, or \"paste_code\" value empty.")
           (error 'pastebin-api-invalid-request :message response))
          ((search "ERROR" response)
           (error 'pastebin-api-error :message response))
          (T #+win32
             (when open-with-default-browser
               (open-with-file-protocol-handler response))
             (parse-uri response)))
        (error 'pastebin-api-error
               :message (format nil "HTTP ~a: ~a"
                                status-code reason-phrase))))))
