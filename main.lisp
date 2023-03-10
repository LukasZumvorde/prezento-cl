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
;; (ql:quickload :cl-argparse)
;; (ql:quickload "unix-opts")

(require "markdown.cl")
(require "parenscript")
(require "css-lite")
(require "cl-who")
(require "quri")
(require :drakma)
(require :cl-base64)
(require :cl-ppcre)
(require :cl-argparse)
(require :unix-opts)

(defpackage :prezento-cl
  (:shadowing-import-from :quri :url-encode :url-decode)
  (:shadowing-import-from :unix-opts :describe)
  (:use
   :common-lisp
   :markdown.cl
   :parenscript
   :cl-who
   :quri
   :drakma
   :cl-base64
   :unix-opts)
  (:export :main))

(in-package :prezento-cl)

(defvar *plugins* nil)

(defvar *input-file* nil)
(defvar *output-file* nil)

(defstruct plugin
  (priority 0.0 :type real)
  (id " " :type string)
  (html " " :type string)
  (css " " :type string)
  (js " " :type string))

(defun compare-plugin-priority (a b)
  (>= (plugin-priority a) (plugin-priority b)))

(defun add-plugin (p)
  (setf *plugins* (push p *plugins*))
  nil)

(defun generate-css ()
  "Creates the CSS code from the *plugins"
  (format nil "~{~A~^~%~}" (map 'list #'plugin-css *plugins*)))

(defun generate-javascript ()
  "Creates the javascript code from the *plugins"
  (format nil "~{~A~^~%~}" (map 'list #'plugin-js *plugins*)))

(defun generate-html-body ()
  "Creates the html code from the *plugins"
  (format nil "~{~A~^~%~}" (map 'list #'plugin-html *plugins*)))

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
  (add-plugin
   (make-plugin
	:priority 100.0
	:html (with-html-output-to-string (s)
			(:div :class "rawinput" (write-string (markdown:parse-file filename) s)))
	:css (css-lite:css
		  (("body")
		   (:margin 0
			:padding 0
			:overflow :hidden
			:height "100%"))))))

(defun plugin-html-from-markdown-string (markdown-string)
  (add-plugin
   (make-plugin
	:priority 100.0
	:html (with-html-output-to-string (s)
			(:div :class "rawinput" (write-string (markdown:parse markdown-string) s)))
	:css (css-lite:css
		  (("body")
		   (:margin 0
			:padding 0
			:overflow :hidden
			:height "100%"))))))

(defun plugin-sort-into-pages ()
  (add-plugin
   (make-plugin
	:priority 75.0
	:js
   "function sortIntoPages(){
		function newSlide(slideid, idx, idy){
			var sec = document.createElement('section');
			sec.classList.add('slide');
			sec.setAttribute('data-slideindex',slideid);
			sec.setAttribute('data-top-level-index', idx);
			sec.setAttribute('data-sub-level-index', idy);
			sec.style['left'] = idx*100+20 + '%';
			sec.style['top'] = idy*100 + 'vh';
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
		var slideid = 0;
		var topLevelIndex = 0;
		var subLevelIndex = 0;
		slidesdiv.appendChild( newSlide(slideid++, topLevelIndex++, subLevelIndex) );
		while(markdowndiv.hasChildNodes()){
			var c = markdowndiv.firstElementChild;
			if( c == null )
				break;
			if(c.matches('h1')){
				if(slidesdiv.lastChild.hasChildNodes()){
                    subLevelIndex = 0;
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
	sortIntoPages();")))

(defun plugin-default-slide-theme ()
  (add-plugin
   (make-plugin
	:priority 95.0
	:css (css-lite:css
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
			:font-family "sans-serif"))))))

(defun plugin-default-js ()
  (add-plugin
   (make-plugin
	:priority 90.0
	:js "const slideChange = new Event('slideChange');")))

(defun plugin-default-css ()
  (add-plugin
   (make-plugin
	:priority 85.0
	:css (css-lite:css (("img")
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
					   (("a")
						(:color "#ee0000"
						 :text-decoration "none"))
					   (("a:link")
						(:color "#ee0000"))
					   (("a:visited")
						(:color "#ee0000"))
					   (("a:hover")
						(:color "#cc0000"))
					   (("a:active")
						(:color "#cc0000"))
					   ))))

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

(defun plugin-slideselect ()
  (add-plugin
   (make-plugin
	:priority 70.0
	:js "
	const nextSlideEvent = new Event('nextSlide');
	const prevSlideEvent = new Event('prevSlide');
	const nextSectionEvent = new Event('nextSection');
	const prevSectionEvent = new Event('prevSection');

	function slideselect(selector) {
		var slides = [];
		var currentSlide = 0;
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
				dispatchEvent(prevSlideEvent);
			else
				dispatchEvent(nextSlideEvent);
		}

		function jumpToSlide(slideindex){
			currentSlide = Math.abs((slideindex+slides.length)%slides.length);
			var h = document.querySelector('[data-slideindex=\"' + currentSlide + '\"]').getAttribute('data-top-level-index');
			var v = document.querySelector('[data-slideindex=\"' + currentSlide + '\"]').getAttribute('data-sub-level-index');
			document.querySelector(selector).style['transform'] = 'translate3d(' + -h*100 + '%,' + -v*100 + 'vh,0)';
			document.querySelector('body').dispatchEvent( new CustomEvent('slideChange', { detail: {currentSlide: currentSlide, maxSlide: slides.length} }) );
		}

		function changeSlide(inc){
			jumpToSlide(currentSlide+inc);
		}

		function changeSection(inc){
			var h = document.querySelector('[data-slideindex=\"' + currentSlide + '\"]').getAttribute('data-top-level-index');
			var newTopLevel = parseInt(h)+inc;
			var newSlide = document.querySelector('[data-top-level-index=\"' + newTopLevel + '\"][data-sub-level-index=\"0\"]');
			if(newSlide == null)
				return;
			var newSlideIndex = parseInt(newSlide.getAttribute('data-slideindex'));
			jumpToSlide(newSlideIndex);
		}

		function jumpByHash(){
			var slideindex = parseInt(window.location.hash.replace('#',''));
			jumpToSlide(slideindex);
		}

		/* check documents ready statement and do init() */
		if(document.readyState === 'complete')
			init();
		else
			window.addEventListener('onload', init(), false);
        window.addEventListener('hashchange', jumpByHash, true);
	}
	slideselect('.slides-container');
"
	:css (css-lite:css
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
		  ))))

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

(defun unknown-option (condition)
  (format t "warning: ~s option is unknown!~%" (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(defun parse-command-line-arguments ()
  ;; declaring the parser
  (opts:define-opts
    (:name :help
           :description "print this help text"
           :short #\h
           :long "help")
    (:name :input
           :description "Input FILE. Set to - or leave out completely for stdin"
           :short #\i
           :long "input"
           :arg-parser #'identity) ;; <- takes an argument
    (:name :output
           :description "Output FILE. Set to - or leave out completely for stdout"
           :short #\o
           :long "output"
           :arg-parser #'identity))
  ;; parsing
  (multiple-value-bind (options free-args)
      (handler-case
		  (handler-bind ((opts:unknown-option #'unknown-option))
			(opts:get-opts))
		(opts:missing-arg (condition)
		  (format t "fatal: option ~s needs an argument!~%"
				  (opts:option condition)))
		(opts:arg-parser-failed (condition)
		  (format t "fatal: cannot parse ~s as argument of ~s~%"
				  (opts:raw-arg condition)
				  (opts:option condition)))
		(opts:missing-required-option (con)
		  (format t "fatal: ~a~%" con)
		  (opts:exit 1)))
	(when-option (options :help)
				 (opts:describe
				  :prefix "A program to generate presentations in HTML format from markdown files. Individual configuration can happen in a front matter containing common lisp code"
				  :suffix ""
				  :usage-of "prezento-cl")
				 (opts:exit 0))
	(when-option (options :input)
				 (setf *input-file* it))
	(when-option (options :output)
				 (setf *output-file* it))
	))

(defun read-input ()
  (cond ((equal "-" *input-file*)
		 (read-markdown-stdin))
		(*input-file*
		 (read-markdown-file *input-file*))
		(t
		 (read-markdown-stdin))))

;; Automatically load all plugins in the plugins directory
(let ((plugins (directory "plugins/*.lisp")))
	(dolist (p plugins)
	  (load p)))

(defun main ()
  (setf *plugins* nil)
  (parse-command-line-arguments)
  (let* ((input (read-input))
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
	(eval (read-from-string (format nil "(progn (in-package :prezento-cl)~A)" config)))
	;; generate html and output
	;; (format t "MAIN ~A~%"         (map 'list #'plugin-priority (sort (copy-list *plugins*) #'> :key #'plugin-priority)))
	;; (format t "MAIN ~A~%"         (map 'list #'plugin-priority *plugins*))
	(setf *plugins* (sort *plugins* #'> :key #'plugin-priority))
	(cond ((equal "-" *output-file*)
		   (format t "~A" (generate-html)))
		  (*output-file*
		   (with-open-file (stream *output-file*)
			 (format stream "~A" (generate-html))))
		  (t
		   (format t "~A" (generate-html)))
		  )
	(format t "~A" (generate-html))
	))

