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
