(defun plugin-progressbar (&key (height "1%") (color "#0000ff"))
  "Add a small line to the bottom of the screen indicating the progess in the presentation."
  (add-to-end *html*
			  (cl-who:with-html-output-to-string (s)
				(:div :class "progressbar")))
  (add-to-end *css*
			  (css-lite:css
				((".progressbar")
				 (:position :fixed
				  :width "100%"
				  :height height
				  :bottom 0
				  :background color
				  :transition "width 1s" ))))
  (add-to-end *js* "
function progressbar() {
	document.querySelector('body').addEventListener('slideChange', function(e){
		document.querySelector('.progressbar').style['width'] = (e.detail.currentSlide / e.detail.maxSlide)*100 + '%';
	});
}
progressbar()"))
