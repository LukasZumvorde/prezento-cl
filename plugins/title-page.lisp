(defun plugin-title-page (&key (title "Title")  (subtitle ""))
  "Add a titlepage to the beginning of the presentation"
  (add-plugin
   (make-plugin
	:priority 76.0
	:js (format nil "
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
var hasTitlePage=true;
" title subtitle))))
