(defun plugin-default-footer (&key (text "Footer text") (bgcolor "#1abc9c") (fgcolor "#ffffff"))
  "Adds the default footer to the slides"
  (add-to-end *html*
			  (with-html-output-to-string (s)
				(:div :class "footer"
					  (:p (write-string text s)))))
  (add-to-end *css*
			  (css-lite:css
			   ((".footer")
				(:position :fixed
				 :left "0"
				 :bottom "0"
				 :width "100%"
				 :background bgcolor
				 :color fgcolor
				 :text-align "center")))))
