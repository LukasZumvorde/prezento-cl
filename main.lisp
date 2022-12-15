(ql:quickload "markdown.cl" :silent t)
(ql:quickload "parenscript":silent t )
(ql:quickload "cl-who" :silent t)
(ql:quickload "hunchentoot" :silent t)
(ql:quickload "css-lite" :silent t)
;; (ql:quickload "3bmd" :silent t)
(ql:quickload :cl-ppcre :silent t)

(defpackage :lukaz-present
  (:use
   :common-lisp
   :markdown.cl
   :parenscript
   :cl-who
   :hunchentoot
   ;; :3bmd
   )
  (:export :main))

(in-package :lukaz-present)

(defvar *css* nil)

(defvar *html* nil)

(defvar *js* nil)

(defvar *acceptor* nil)

(defmacro add-to-front (l e)
  "Add element E to the front of list L"
  `(setf ,l (cons ,e ,l)))

(defmacro add-to-end (l e)
  "Add element E to the end of list L"
  `(setf ,l (append ,l (list ,e))))

(defun generate-css ()
  "Creates the CSS code from the *css* variable."
  (format nil "~{~A~^~%~}" *css*))

(defun generate-javascript ()
  "Creates the javascript code from the *js* variable."
  (format nil "~{~A~^~%~}" *js*))

(defun generate-html-body ()
  "Creates the html from the *html* variable."
  (format nil "~{~A~^~%~}" *html*))

(defun generate-html ()
  "Generates the entire html string and returns it. All plugins have to be applied before applying this function"
  (with-html-output-to-string (s nil :prologue t)
      (:html
       (:head (:title "Title for the Presentation")
			  (:style (write-string (generate-css) s)))
       (:body (write-string (generate-html-body) s)
			  (:script (write-string (generate-javascript) s))))))

(defun plugin-html-from-markdown-file (filename)
  "Adds the html generated from the markdown file to the slides"
  (add-to-front *html*
				(with-html-output-to-string (s)
				  (:div :class "rawinput" (write-string (markdown:parse-file filename) s))))
  (add-to-front *css*
				(css-lite:css
				  (("body")
				   (:margin 0
					:padding 0
					:overflow :hidden
					:height "100%")))))

(defun plugin-html-from-markdown-string (markdown-string)
  "Adds the html generated from the markdown string to the slides"
  (add-to-front *html*
				(with-html-output-to-string (s)
				  (:div :class "rawinput" (write-string (markdown:parse markdown-string) s))))
  (add-to-front *css*
				(css-lite:css
				  (("body")
				   (:margin 0
					:padding 0
					:overflow :hidden
					:height "100%")))))

(defun plugin-default-header ()
  "Adds the default header to the slides"
  (add-to-front *html*
				(with-html-output-to-string (s)
				  (:div :class "header"
						;;(:h5 "Header")
						(:p "My supercool header"))))
  (add-to-end *css*
			  (css-lite:css
			   ((".header")
				(;; :position :fixed
				 ;; :left "0"
				 ;; :top "0"
				 ;; :width "100%"
				 :padding "20px"
				 :text-align "center"
				 :background "#1abc9c"
				 :color :white
				 ;; :font-size "16px"
				 )))))

(defun plugin-default-footer ()
  "Adds the default footer to the slides"
  (add-to-end *html*
			  (with-html-output-to-string (s)
				(:div :class "footer"
					  ;;(:h5 "Footer")
					  (:p "A not so cool footer"))))
  (add-to-end *css*
			  (css-lite:css
			   ((".footer")
				(:position :fixed
				 :left "0"
				 :bottom "0"
				 :width "100%"
				 :background :red
				 :color :white
				 :text-align "center"
				 )))))

(defun plugin-sort-into-pages ()
  (add-to-front
   *js*
   "function sortIntoPages(){
		function newSlide(idx){
			var sec = document.createElement('section');
			sec.classList.add('slide');
			sec.setAttribute('data-slideindex',idx);
			sec.style['left'] = (idx-1)*100 + '%';
			sec.style['position'] = 'absolute';
			return sec;
		}
		// div that the markdown input is delivered in
		var markdowndiv = document.querySelector('.rawinput');
		// div that we want to put the pages is
		var pagesdiv = document.createElement('div');
		pagesdiv.classList.add('pages');
		pagesdiv.classList.add('slides-container');
		markdowndiv.after(pagesdiv);
		// move contents to pages
		var index = 1;
		pagesdiv.appendChild( newSlide(index++) );
		while(markdowndiv.hasChildNodes()){
			var c = markdowndiv.firstElementChild;
			if( c == null )
				break;
			if(c.matches('h1')){
				if(pagesdiv.lastChild.hasChildNodes()){
					pagesdiv.appendChild( newSlide(index++) );
					// pagesdiv.appendChild( document.createElement('section') );
				}
			}
			pagesdiv.lastChild.appendChild(c);
		}
		// remove rawinput container
		markdowndiv.remove();
	}
	sortIntoPages();"))

(defun plugin-default-slide-theme ()
  (add-to-front *css*
				(css-lite:css
				  ((".ops-page")
				   (:text-align :center))
				  (("h1")
				   (:font-size "1.75em"
					:font-family "sans-serif"
					:color "#ee0000"
					:text-transform :uppercase
					:hyphens :auto
					:word-wrap :break-word))
				  (("h2")
				   (:font-size "1.5em"
					:font-family "sans-serif"
					:color "#ee0000"))
				  (("h3")
				   (:font-size "1.25em"
					:font-family "sans-serif"
					:color "#ee0000"))
				  (("body")
				   (:font-size "200%"
					:font-family "sans-serif")))))

(defun plugin-default-js ()
  (add-to-front *js* "
const slideChange = new Event('slideChange');
"))

(defun plugin-default-css ()
  (add-to-front *css*
				(css-lite:css (("img")
							   (:width "50%"))
							  (("ul")
							   (:text-align "left"
								:display "inline-block"))
							  (("li > ul")
							   (:display "block")))))

(defun start-webserver ()
  (setq cl-who:*attribute-quote-char* #\")
  (when *acceptor*
	(stop *acceptor*))
  (setf (who:html-mode) :html5)
  (setf *acceptor* (start (make-instance 'hunchentoot:easy-acceptor :port 5000))))

(defun save-to-file (filename html-string)
  "Save the HTML-STRING to the file FILENAME"
  (with-open-file (stream filename
						  :direction :output
						  :if-exists :overwrite
						  :if-does-not-exist :create)
				  (format stream html-string)))

(defun output-to-stdout (html-string)
  "Output the HTML-STRING to standard out"
  (format nil html-string))

(defun serve-with-webserver (html-string)
  "Serve the webpage with the HTML-STRING"
  (define-easy-handler (preview :uri "/") () html-string))

(defun plugin-progressbar (&key (height "1%") (color "#0000ff"))
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
				  :background color))))
  (add-to-end *js* "
function progressbar() {
	document.querySelector('body').addEventListener('slideChange', function(e){
		document.querySelector('.progressbar').style['width'] = (e.detail.currentSlide / e.detail.maxSlide)*100 + '%';
	});
}
progressbar()"))


(defun plugin-slideselect ()
  (add-to-end
   *js* "
	function slideselect(selector, options) {
		var pages = [];
		var currentPage = 1;
		var keyPrev = {38:1,33:1,37:1};
		var keyNext = {40:1,34:1,39:1};

		var def = {
			pageContainer: 'section',
			infinite: true,
			keyboard: true,
			direction: 'vertical',
		};

		/* extend function for user customization */
		function extend(){
			for(var i=1; i<arguments.length; i++)
				for(var key in arguments[i])
					if(arguments[i].hasOwnProperty(key))
						arguments[0][key] = arguments[i][key];
			return arguments[0];
		}

		var setting = extend({},def,options);

		/* initialization */
		function init(){
			window.addEventListener('wheel',onScrollEventHandler);

			//allow keyboard input
			if(setting.keyboard){
				addEventListener('keydown', function(e){
					if(keyPrev[e.keyCode])
						changePage(1,pages.length,-1);
					else if(keyNext[e.keyCode])
						changePage(pages.length,1,1);
				});
			}

			var index=1;
			[].forEach.call(document.querySelectorAll(selector + ' > ' + setting.pageContainer), function(obj){
				pages.push(obj);
			});
		}

		/* wheel event handler */
		function onScrollEventHandler(e){
			if(e.wheelDelta > 0)
				changePage(1,pages.length,-1);
			else
				changePage(pages.length,1,1);
		}

		//function for page transition
		function changePage(compare,edge,increase){
			if(currentPage==compare){
				if(setting.infinite)
					currentPage = edge;
				else
					return;
			} else {
				currentPage+=increase;
			}

			if(setting.direction == 'vertical')
				document.querySelector(selector).style['transform'] = 'translate3d(0,' + -(currentPage-1)*100 + '%,0)';
			else if(setting.direction == 'horizontal')
				document.querySelector(selector).style['transform'] = 'translate3d(' + -(currentPage-1)*100 + '%,0,0)';
			document.querySelector('body').dispatchEvent( new CustomEvent('slideChange', { detail: {currentSlide: currentPage, maxSlide: pages.length} }) );
		}

		/* check documents ready statement and do init() */
		if(document.readyState === 'complete')
			init();
		else
			window.addEventListener('onload', init(), false);
	}
	slideselect(\".pages\",{pagination: false , direction: 'horizontal'});")
  (add-to-end
   *css*
   (css-lite:css
	 ((".slides-container")
	  (:position :relative
	   :display :block
	   :padding 0
	   :margin 0
	   :height "100%"
	   :width "100%"
	   :transform "translate3d(0,0,0)"))
	 ((".slide")
	  (:width "100%"
	   :height "100%"
	   :position :relative
	   :text-align :center))
	 )))


(defun update ()
  (setf *html* nil)
  (setf *css* nil)
  (setf *js* nil)
  (plugin-html-from-markdown-string "
# Heading 1

Text on slide 1

## Subheading 1.1

- bullet
- list

# Heading 2

Text on slide 2

# Heading 3

Text on slide 3

Somethimes we have very long paragraphs, whose lines are much much longer than we normaly want to get. Well on slow browsers the show/hide method MIGHT cause the box to flicker (though the computer have to be really slow). So if you want to avoid this, give the div a opacity: 0 - and perhaps even a position: absolute, so it doesnt push the content. So to extend the code from before.


# Heading 4

Text on slide 4

![Dogs Image](https://cdn.theatlantic.com/thumbor/GtkxlReLLoEz2f-mJz7591LXHnM=/0x104:2000x1229/1920x1080/media/img/2016/06/01/atlantic_full/original.jpg)

<img src=\"https://cdn.theatlantic.com/thumbor/GtkxlReLLoEz2f-mJz7591LXHnM=/0x104:2000x1229/1920x1080/media/img/2016/06/01/atlantic_full/original.jpg\" style=\"width: 200px\" />

More text

# Heading 5

- Items
- Are
  - not
  - all
  - on
- the same level

# Heading 6

Text

## Subheading 6.1

Text

## Subheading 6.2

Text

## Subheading 6.3

Text

")
  (plugin-default-slide-theme)
  (plugin-default-js)
  (plugin-default-header)
  (plugin-default-footer)
  (plugin-sort-into-pages)
  (plugin-slideselect)
  (plugin-progressbar)
  (serve-with-webserver (generate-html)))

(defun read-markdown-stream (stream)
  "Reads the markdown from STREAM and returns a list with the front-matter as the first element and the markdown document as the second and last element"
  (loop :for line = (read-line stream nil)
		:while line
		:count t :into line-count
		:if (and (= line-count 1) (equal "---" line)) :count t :into page-breaks :else
		  :if (and (> line-count 1) (equal "---" line) (= page-breaks 1)) :count t :into page-breaks :else
			:if (= page-breaks 1) :collect line :into front-matter :else
			  :collect line :into end-matter
		:finally (return (list (format nil "~{~A~^~%~}" front-matter)
							   (format nil "~{~A~^~%~}" end-matter)))))

(defun read-markdown-file (filename)
  "Reads the markdown from FILENAME and returns a list with the front-matter as the first element and the markdown document as the second and last element"
  (with-open-file (stream filename)
	(read-markdown-stream stream)))

(defun read-markdown-string (string)
  "Reads the markdown from STRING and returns a list with the front-matter as the first element and the markdown document as the second and last element"
	(read-markdown-stream (make-string-input-stream string)))

(defun read-markdown-stdin ()
  "Reads the markdown from standard in and returns a list with the front-matter as the first element and the markdown document as the second and last element"
  (read-markdown-stream *standard-input*))

;; ;; (plugin-1 arg1 arg2 arg3 ...)
;; ;; (plugin-2 ...)
;; ;; ...
;; (defmacro apply-config (cfg)
;;   "Take in config CFG as a list and apply the settings. This mostly means calling the right plugins. The config is normaly what is encoded in the front matter of the markdown document"
;;   `(read-from-string (format nil "(progn ~A)" ,cfg)))

(defun main ()
  (setf *html* nil)
  (setf *css* nil)
  (setf *js* nil)
  (let* ((input (read-markdown-stdin))
		 (config (first input))
		 (markdown (second input)))
	;; markdown and always on plugins
	(plugin-html-from-markdown-string markdown)
	(plugin-default-slide-theme)
	(plugin-default-js)
	(plugin-default-css)
	(plugin-sort-into-pages)
	(plugin-slideselect)
	;; custom plugin selection
	(eval (read-from-string (format nil "(progn ~A)" config)))
	;; generate html and output
	(format t "~A" (generate-html))))

;;(start-webserver)
;;(update)

