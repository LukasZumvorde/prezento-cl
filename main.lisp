;; (ql:quickload "markdown.cl" :silent t)
;; (ql:quickload "parenscript":silent t )
;; (ql:quickload "cl-who" :silent t)
;; (ql:quickload "hunchentoot" :silent t)
;; (ql:quickload "css-lite" :silent t)
;; ;; (ql:quickload "3bmd" :silent t)
;; (ql:quickload :cl-ppcre :silent t)
;; (ql:quickload :quri :silent t)

(require "markdown.cl")
(require "parenscript")
(require "css-lite")
(require "cl-who")
(require "hunchentoot")
(require "quri")

(defpackage :lukaz-present
  (:shadowing-import-from :quri :url-encode :url-decode)
  (:use
   :common-lisp
   :markdown.cl
   :parenscript
   :cl-who
   :hunchentoot
   :quri)
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

(defun plugin-sort-into-pages ()
  (add-to-front
   *js*
   "function sortIntoPages(){
		function newSlide(slideid, idx, idy){
			var sec = document.createElement('section');
			sec.classList.add('slide');
			sec.setAttribute('data-slideindex',slideid);
            sec.setAttribute('data-top-level-index', idx);
            sec.setAttribute('data-sub-level-index', idy);
			sec.style['left'] = (idx-1)*100+20 + '%';
            sec.style['top'] = (idy-1)*100 + 'vh';
			sec.style['position'] = 'absolute';
			return sec;
		}
		// div that the markdown input is delivered in
		var markdowndiv = document.querySelector('.rawinput');
		// div that we want to put the slides in
		var slidesdiv = document.createElement('div');
		slidesdiv.classList.add('slides-container');
		markdowndiv.after(slidesdiv);
		// move contents to slides
        var slideid = 1;
		var topLevelIndex = 1;
        var subLevelIndex = 1;
		slidesdiv.appendChild( newSlide(slideid++, topLevelIndex++, subLevelIndex) );
		while(markdowndiv.hasChildNodes()){
			var c = markdowndiv.firstElementChild;
			if( c == null )
				break;
			if(c.matches('h1')){
				if(slidesdiv.lastChild.hasChildNodes()){
                    subLevelIndex = 1;
					slidesdiv.appendChild( newSlide(slideid++, topLevelIndex++, subLevelIndex) );
				}
			}
            if(c.matches('h2')){
                if(slidesdiv.lastChild.hasChildNodes()){
					slidesdiv.appendChild( newSlide(slideid++, (topLevelIndex-1), 1+subLevelIndex++) );
				}
            }
			slidesdiv.lastChild.appendChild(c);
		}
		// remove rawinput container
		markdowndiv.remove();
	}
	sortIntoPages();"))

(defun plugin-default-slide-theme ()
  (add-to-front *css*
				(css-lite:css
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
							   (:width "100%"))
							  (("ul")
							   (:text-align "left"
								:display "inline-block"))
							  (("li > ul")
							   (:display "block"))
							  (("table")
							   (:margin-left "auto"
								:margin-right "auto"
								:text-align "left"))
							  (("table, th, td")
							   (:border "1px solid black"
								:border-collapse "collapse"))
							  (("tr:nth-child(odd)")
							   (:background-color "#fafafa"))
							  (("tr:nth-child(even)")
							   (:background-color "#dddddd"))
							  (("th")
							   (:background-color "#cccccc"))
							  )))

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


(defun plugin-slideselect ()
  (add-to-end
   *js* "
	function slideselect(selector) {
		var slides = [];
		var currentSlide = 1;
		var keyPrev = {38:1,33:1,37:1};
		var keyNext = {40:1,34:1,39:1};

		/* initialization */
		function init(){
			window.addEventListener('wheel',onScrollEventHandler);

			addEventListener('keydown', function(e){
				if(keyPrev[e.keyCode])
					changeSlide(-1);
				else if(keyNext[e.keyCode])
					changeSlide(1);
			});

			[].forEach.call(document.querySelectorAll('.slides-container > section'), function(obj){
				slides.push(obj);
			});
		}

		/* wheel event handler */
		function onScrollEventHandler(e){
			if(e.wheelDelta > 0)
				changeSlide(-1);
			else
				changeSlide(1);
		}

		function changeSlide(inc){
			currentSlide = Math.abs( (currentSlide-1+inc+slides.length)%slides.length) + 1 ;
			var h = document.querySelector('[data-slideindex=\"' + currentSlide + '\"]').getAttribute('data-top-level-index');
			var v = document.querySelector('[data-slideindex=\"' + currentSlide + '\"]').getAttribute('data-sub-level-index');
			document.querySelector(selector).style['transform'] = 'translate3d(' + -(h-1)*100 + '%,' + -(v-1)*100 + 'vh,0)';
			document.querySelector('body').dispatchEvent( new CustomEvent('slideChange', { detail: {currentSlide: currentSlide, maxSlide: slides.length} }) );
		}

		/* check documents ready statement and do init() */
		if(document.readyState === 'complete')
			init();
		else
			window.addEventListener('onload', init(), false);
	}
	slideselect('.slides-container');")
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
	   :transform "translate3d(0,0,0)"
	   :transition "transform 1s"))
	 ((".slide")
	  (:width "60%"
	   :height "100%"
	   :position :relative
	   :text-align :center))
	 )))

;; (defun get-src-from-img-tag (html-string img-tag-start img-tag-end)
;;   "Return the "
;;   (multiple-value-bind (s e)
;; 	  (cl-ppcre:scan " src=\"[^ ]*\"" html-string :start img-tag-start :end img-tag-end)
;; 	(values (+ s 6) (- e 1))))

;; (defun plugin-self-contain-images ()
;;   "Converts strings to images in the presentation. They are downloaded and then encoded in base64. The base64 encoded image is then diectly placed in the html."
;;   (let ((html-string "<p><img src='https://cdn.theatlantic.com/thumbor/GtkxlReLLoEz2f-mJz7591LXHnM=/0x104:2000x1229/1920x1080/media/img/2016/06/01/atlantic_full/original.jpg' alt='Dogs Image'></p>"))
;; 	;; for every image do ...
;; 	(cl-ppcre:do-matches (s
;; 						  e
;; 						  "<img src=[^ ]* "
;; 						  html-string
;; 						  nil)
;; 	  (format t "~A~%" (subseq html-string (+ s 10) (- e 2))))))

(defun plugin-title-page (&key (title "Title")  (subtitle ""))
  "Add a titlepage to the beginning of the presentation"
  (add-to-front *js*
				(format nil "
function titlepage() {
  var place = document.querySelector('div.rawinput');
  var title = document.createElement('h1');
  title.textContent = '~A';
  var subtitle = document.createElement('p');
  subtitle.textContent = '~A';
  place.insertBefore(subtitle, place.firstChild);
  place.insertBefore(title, place.firstChild);
}
titlepage();
" title subtitle)))

(defun plugin-table-of-contents (&key (heading "Table of Contents"))
  "Add a titlepage to the beginning of the presentation"
  (add-to-front *js*
				(format nil "
function tableofcontent() {
  var place = document.querySelector('div.rawinput');
  var heading = document.createElement('h1');
  heading.textContent = '~A';
  var list = document.createElement('ul');
  place.insertBefore(list, place.firstChild);
  [].forEach.call(document.querySelectorAll('div.rawinput > h1'), function(obj){
    var listelement = document.createElement('li');
    listelement.textContent = obj.textContent;
    list.appendChild(listelement);
  });
  place.insertBefore(heading, place.firstChild);
}
tableofcontent();
" heading)))

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


