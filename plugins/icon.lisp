(defun plugin-icon (img-src &key top bottom left right width height)
  (add-plugin
   (make-plugin
	:priority 50.0
	:html (cl-who:with-html-output-to-string (s)
			(:div :class "icon"
				  (:img :src img-src s)))
	:css (css-lite:css
		  ((".icon")
		   (:position :fixed
			:top top
			:bottom bottom
			:left left
			:right right
			:width width
			:height height))))))
