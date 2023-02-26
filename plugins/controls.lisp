(defun plugin-controls (&key (fill-color "#D35F5E") (stroke-color "#000000") (stroke-width "0"))
  (add-plugin
   (make-plugin
	:priority 85.0
	:html (cl-who:with-html-output-to-string (s)
			(:div :class "controls"
				  "
<svg version=\"1.1\"
     baseProfile=\"full\"
     width=\"100\" height=\"100\"
     xmlns=\"http://www.w3.org/2000/svg\">
	 <g fill=\""(write-string fill-color s)"\"
	 	style=\"fill:"(write-string fill-color s)";stroke:"(write-string stroke-color s)";stroke-width:"(write-string stroke-width s)";\">
	   <polygon points=\"30,20 50,0 70,20\"
				onclick=\"window.dispatchEvent(nextSectionEvent)\"/>
	   <polygon points=\"80,30 100,50 80,70\"
				onclick=\"window.dispatchEvent(nextSlideEvent)\"/>
	   <polygon points=\"30,80 70,80 50,100\"
				onclick=\"window.dispatchEvent(prevSectionEvent)\"/>
	   <polygon points=\"0,50 20,30 20,70\"
				onclick=\"window.dispatchEvent(prevSlideEvent)\"/>
	 </g>
</svg>" s))
	:css (css-lite:css
		  ((".controls")
		   (:position :fixed
			:bottom 0
			:right 0))
		  ((".controls:hover")
		   (:fill "#FFFFFF"))))))
