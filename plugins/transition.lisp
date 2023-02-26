(defun plugin-transition (&key (time "1s"))
  "Change the time the slide transition takes. Set it to \"0s\" to disable it."
  (add-plugin
   (make-plugin
	:priority 50.0
	:css (css-lite:css ((".slides-container")
						(:transition time))))))
