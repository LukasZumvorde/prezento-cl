(defun plugin-default-header (&key (text "Header text") (bgcolor "#1abc9c") (fgcolor "#ffffff"))
  "Adds the default header to the slides"
  (add-to-front *html*
				(with-html-output-to-string (s)
				  (:div :class "header"
						(:p (write-string text s)))))
  (add-to-end *css*
			  (css-lite:css
			   ((".header")
				(:padding "20px"
				 :text-align "center"
				 :background bgcolor
				 :color fgcolor )))))
