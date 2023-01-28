(defun plugin-transition (&key (time "1s"))
  "Change the time the slide transition takes. Set it to \"0s\" to disable it."
  (css-append (css-lite:css ((".slides-container")
							 (:transition time)))))
