;; (ql:quickload "markdown.cl" :silent t)
;; (ql:quickload "parenscript":silent t )
;; (ql:quickload "cl-who" :silent t)
;; (ql:quickload "css-lite" :silent t)
;; ;; (ql:quickload "3bmd" :silent t)
;; (ql:quickload :cl-ppcre :silent t)
;; (ql:quickload :quri :silent t)
;; (ql:quickload :drakma)
;; (ql:quickload :cl-base64)
;; (ql:quickload :cl-ppcre)


(require "markdown.cl")
(require "parenscript")
(require "css-lite")
(require "cl-who")
(require "quri")
(require :drakma)
(require :cl-base64)
(require :cl-ppcre)

(defpackage :lukaz-present
  (:shadowing-import-from :quri :url-encode :url-decode)
  (:use
   :common-lisp
   :markdown.cl
   :parenscript
   :cl-who
   :quri
   :drakma
   :cl-base64)
  (:export :main))

(in-package :lukaz-present)

(defvar *css* nil)

(defvar *html* nil)

(defvar *js* nil)

(defvar *acceptor* nil)

(defvar *iconcounter* 0)

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
								:text-align "left"
								:font-size "1em"))
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

	const nextSlideEvent = new Event('nextSlide');
	const prevSlideEvent = new Event('prevSlide');
	const nextSectionEvent = new Event('nextSection');
	const prevSectionEvent = new Event('prevSection');

	function slideselect(selector) {
		var slides = [];
		var currentSlide = 1;
		var keyPrev = {38:1,33:1,37:1};
		var keyNext = {40:1,34:1,39:1};

		/* initialization */
		function init(){
			window.addEventListener('wheel',onScrollEventHandler);

			addEventListener('nextSlide', function(e){ changeSlide(1); });
			addEventListener('prevSlide', function(e){ changeSlide(-1); });
			addEventListener('nextSection', function(e){ changeSection(1); });
			addEventListener('prevSection', function(e){ changeSection(-1); });

			addEventListener('keydown', function(e){
				if(e.keyCode == 33)
					changeSlide(-1);
				if(e.keyCode == 37)
					changeSlide(-1);
				if(e.keyCode == 38)
					changeSection(1);
				if(e.keyCode == 40)
					changeSection(-1);
				if(e.keyCode == 34)
					changeSlide(1);
				if(e.keyCode == 39)
					changeSlide(1);
				/* if(keyPrev[e.keyCode])
					changeSlide(-1);
				else if(keyNext[e.keyCode])
					changeSlide(1); */
			});

			[].forEach.call(document.querySelectorAll('.slides-container > section'), function(obj){
				slides.push(obj);
			});
		}

		/* wheel event handler */
		function onScrollEventHandler(e){
			if(e.wheelDelta > 0)
				dispatchEvent(nextSlideEvent);
			else
				dispatchEvent(prevSlideEvent);
		}

		function changeSlide(inc){
			currentSlide = Math.abs( (currentSlide-1+inc+slides.length)%slides.length) + 1 ;
			var h = document.querySelector('[data-slideindex=\"' + currentSlide + '\"]').getAttribute('data-top-level-index');
			var v = document.querySelector('[data-slideindex=\"' + currentSlide + '\"]').getAttribute('data-sub-level-index');
			document.querySelector(selector).style['transform'] = 'translate3d(' + -(h-1)*100 + '%,' + -(v-1)*100 + 'vh,0)';
			document.querySelector('body').dispatchEvent( new CustomEvent('slideChange', { detail: {currentSlide: currentSlide, maxSlide: slides.length} }) );
		}

		function changeSection(inc){
			var h = document.querySelector('[data-slideindex=\"' + currentSlide + '\"]').getAttribute('data-top-level-index');
			var newTopLevel = parseInt(h)+inc;
			var newSlide = document.querySelector('[data-top-level-index=\"' + newTopLevel + '\"]');
			if(newSlide == null)
				return;
			var newSlideIndex = newSlide.getAttribute('data-slideindex');
			currentSlide = newSlideIndex;
			var h = document.querySelector('[data-slideindex=\"' + newSlideIndex + '\"]').getAttribute('data-top-level-index');
			var v = document.querySelector('[data-slideindex=\"' + newSlideIndex + '\"]').getAttribute('data-sub-level-index');
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

(defun plugin-vertically-center-slides ()
  (add-to-end *js*
			  "
function verticallycenterslides() {
  [].forEach.call(document.querySelectorAll('.slide'), function(obj){
    var offset = Math.floor(Math.max(window.innerHeight - obj.scrollHeight,0) / 2);
    obj.style['padding'] = '' + 100 * offset / window.innerHeight + 'vh 0';
  });
}
verticallycenterslides();
"))

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

(defun plugin-controls (&key (fill-color "#D35F5E") (stroke-color "#000000") (stroke-width "0"))
  (add-to-end *html*
			  (cl-who:with-html-output-to-string (s)
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
</svg>" s)))
  (add-to-end *css* (css-lite:css
					 ((".controls")
					  (:position :fixed
					   :bottom 0
					   :right 0))
					  ((".controls:hover")
					   (:fill "#FFFFFF")))))

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

(defun load-all-plugins ()
  "Load all lisp files in the plugins directory"
  (let ((plugins (directory "plugins/*.lisp")))
	(dolist (p plugins)
	  (load p))))

(defun main ()
  (setf *html* nil)
  (setf *css* nil)
  (setf *js* nil)
  (load-all-plugins)
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





(defun get-base64-encoded-stream-contents (stream)
  (loop with array = (make-array 0 :element-type '(unsigned-byte 8)
								   :adjustable t :fill-pointer 0)
		for byte = (read-byte stream nil)
		while byte
		do (vector-push-extend byte array)
		finally (return (cl-base64:usb8-array-to-base64-string array))))

(defun get-base64-encoded-uri-contents (uri)
  (get-base64-encoded-stream-contents (drakma:http-request uri :want-stream t)))

(defun get-base64-encoded-file-contents (path)
  (with-open-file (stream path :element-type '(unsigned-byte 8))
	(get-base64-encoded-stream-contents stream)))

(defmacro try-these (default &body body)
  "Try to execute each command in order. If an error happens try the next. If no error happens exit."
  (if body
	  (let ((next (car body))
			(rest (cdr body)))
		`(handler-case ,next
		   (t ()
			 (try-these ,default
						 ,@rest))))
	  default))

(defun get-base64-encoded-image-contents (string)
  (try-these nil
	(get-base64-encoded-file-contents string)
	(get-base64-encoded-uri-contents string)))

(defun find-img-tag (text &optional (start 0))
  (multiple-value-bind (s e)
	  (cl-ppcre:scan "<img [^>]*>" text :start start)
	(cond ((null s) nil)
		  ((null e) nil)
		  (t (list s e (subseq text s e))))))

(defun find-src-url-in-img (text start end)
  (multiple-value-bind (s e)
	  (cl-ppcre:scan " src=['\"](?!data:image/)[^ ]*['\"]" text :start start :end end)
	(cond ((null s) nil)
		  ((null e) nil)
		  (t (list (+ 6 s) (- e 1) (subseq text (+ 6 s) (- e 1)))))))

(defun file-extension (url)
  "Return the file extension if any. Return nil otherwise"
  (multiple-value-bind (s e)
	  (cl-ppcre:scan "\\.[^./]+$" url)
	(if (and s e)
		(cond ((equal "svg" (subseq url (+ 1 s) e)) "svg+xml")
			  (t (subseq url (+ 1 s) e)))
		nil)))

(defun replace-src-url (text start end)
  (let* ((url (subseq text start end))
		 (base64img (get-base64-encoded-image-contents url))
		 (extension (file-extension url)))
	(if (and url base64img extension)
		(cl-ppcre:regex-replace (subseq text start end) text (format nil "data:image/~A;base64,~A" extension base64img))
		text)))

(defun inline-images (html-string &optional (start 0))
  (let ((img-search (find-img-tag html-string start)))
	(if img-search
		(let ((src-url-search (find-src-url-in-img html-string (first img-search) (second img-search))))
		  (if src-url-search
			  (let ((inline-html-string (replace-src-url html-string (first src-url-search) (second src-url-search))))
				(if inline-html-string
					(inline-images inline-html-string (+ 1 (first img-search)))
					(inline-images html-string (+ 1 (first img-search)))
					)
				)
			  (inline-images html-string (+ 1 (first img-search)))
			  )
		  )
		html-string)
	)
  )

(defun plugin-inline-images ()
  "Replace all images in the html into images that are embedded in the html code"
  (setq *html* (map 'list #'inline-images *html*)))



