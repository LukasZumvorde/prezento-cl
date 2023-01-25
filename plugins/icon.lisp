(defun plugin-icon (img-src &key top bottom left right width height)
  (add-to-end *html*
			  (cl-who:with-html-output-to-string (s)
					(:div :class "icon"
						  (:img :src img-src s))))
  (add-to-end *css* (css-lite:css
					 ((".icon")
					  (:position :fixed
					   :top top
					   :bottom bottom
					   :left left
					   :right right
					   :width width
					   :height height)))))
