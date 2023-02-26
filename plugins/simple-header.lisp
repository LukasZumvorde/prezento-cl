(defun plugin-default-header (&key (text "Header text") (bgcolor "#1abc9c") (fgcolor "#ffffff"))
  "Adds the default header to the slides"
  (add-plugin
   (make-plugin
	:priority 50.0
	:html (with-html-output-to-string (s)
			(:div :class "header"
				  (:p (write-string text s))))
	:css (css-lite:css
		  ((".header")
		   (:padding "20px"
			:text-align "center"
			:background bgcolor
			:color fgcolor ))))))
