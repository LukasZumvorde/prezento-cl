(defun plugin-default-footer (&key (text "Footer text") (bgcolor "#1abc9c") (fgcolor "#ffffff"))
  "Adds the default footer to the slides"
  (add-plugin
   (make-plugin
	:priority 50.0
	:html (with-html-output-to-string (s)
			(:div :class "footer"
				  (:p (write-string text s))))
	:css (css-lite:css
		  ((".footer")
		   (:position :fixed
			:left "0"
			:bottom "0"
			:width "100%"
			:background bgcolor
			:color fgcolor
			:text-align "center"))))))
