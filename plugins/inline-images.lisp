

(defun get-base64-encoded-stream-contents (stream)
  (loop with array = (make-array 0 :element-type '(unsigned-byte 8)
								   :adjustable t :fill-pointer 0)
		for byte = (read-byte stream nil)
		while byte
		do (vector-push-extend byte array)
		finally (return (cl-base64:usb8-array-to-base64-string array))))

(defun get-base64-encoded-uri-contents (uri)
  (get-base64-encoded-stream-contents (drakma:http-request uri :want-stream t)))

(defun get-base64-encoded-file-contents (path)
  (with-open-file (stream path :element-type '(unsigned-byte 8))
	(get-base64-encoded-stream-contents stream)))

(defmacro try-these (default &body body)
  "Try to execute each command in order. If an error happens try the next. If no error happens exit."
  (if body
	  (let ((next (car body))
			(rest (cdr body)))
		`(handler-case ,next
		   (t ()
			 (try-these ,default
						 ,@rest))))
	  default))

(defun get-base64-encoded-image-contents (string)
  (try-these nil
	(get-base64-encoded-file-contents string)
	(get-base64-encoded-uri-contents string)))

(defun find-img-tag (text &optional (start 0))
  (multiple-value-bind (s e)
	  (cl-ppcre:scan "<img [^>]*>" text :start start)
	(cond ((null s) nil)
		  ((null e) nil)
		  (t (list s e (subseq text s e))))))

(defun find-src-url-in-img (text start end)
  (multiple-value-bind (s e)
	  (cl-ppcre:scan " src=['\"](?!data:image/)[^ ]*['\"]" text :start start :end end)
	(cond ((null s) nil)
		  ((null e) nil)
		  (t (list (+ 6 s) (- e 1) (subseq text (+ 6 s) (- e 1)))))))

(defun file-extension (url)
  "Return the file extension if any. Return nil otherwise"
  (multiple-value-bind (s e)
	  (cl-ppcre:scan "\\.[^./]+$" url)
	(if (and s e)
		(cond ((equal "svg" (subseq url (+ 1 s) e)) "svg+xml")
			  (t (subseq url (+ 1 s) e)))
		nil)))

(defun replace-src-url (text start end)
  (let* ((url (subseq text start end))
		 (base64img (get-base64-encoded-image-contents url))
		 (extension (file-extension url)))
	(if (and url base64img extension)
		(cl-ppcre:regex-replace (subseq text start end) text (format nil "data:image/~A;base64,~A" extension base64img))
		text)))
(defun inline-images (html-string &optional (start 0))
  (let ((img-search (find-img-tag html-string start)))
	(if img-search
		(let ((src-url-search (find-src-url-in-img html-string (first img-search) (second img-search))))
		  (if src-url-search
			  (let ((inline-html-string (replace-src-url html-string (first src-url-search) (second src-url-search))))
				(if inline-html-string
					(inline-images inline-html-string (+ 1 (first img-search)))
					(inline-images html-string (+ 1 (first img-search)))
					)
				)
			  (inline-images html-string (+ 1 (first img-search)))
			  )
		  )
		html-string)
	)
  )

(defun plugin-inline-images ()
  "Replace all images in the html into images that are embedded in the html code"
  (add-plugin
   (make-plugin
	:priority 50.0
	;; :html (map 'list #'inline-images *html*)))
  )))

