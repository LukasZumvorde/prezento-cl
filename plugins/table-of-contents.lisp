(defun plugin-table-of-contents (&key (heading "Table of Contents"))
  "Add a titlepage to the beginning of the presentation"
  (add-plugin
   (make-plugin
	:priority 77.0
	:js (format nil "
function tableofcontent() {
  var place = document.querySelector('div.rawinput');
  var heading = document.createElement('h1');
  var idcounter = 2;
  heading.textContent = '~A';
  // heading.setAttribute('id',idcounter++);
  var list = document.createElement('ul');
  place.insertBefore(list, place.firstChild);
  var lastlevel = 0;
  var sublist;
  [].forEach.call(document.querySelectorAll('div.rawinput > h1,h2'), function(obj){
    if(obj.nodeName.toLowerCase() === 'h1'){
      lastlevel = 0;
      var listelement = document.createElement('li');
      var linkelement = document.createElement('a');
      linkelement.setAttribute('href', '#' + idcounter++);
      linkelement.textContent = obj.textContent;
      list.appendChild(listelement);
      listelement.appendChild(linkelement);
    };
    if(obj.nodeName.toLowerCase() === 'h2'){
      if(lastlevel === 0){
        sublist = document.createElement('ul');
        list.appendChild(sublist);
      };
      var listelement = document.createElement('li');
      var linkelement = document.createElement('a');
      linkelement.setAttribute('href', '#' + idcounter++);
      linkelement.textContent = obj.textContent;
      sublist.appendChild(listelement);
      listelement.appendChild(linkelement);
      lastlevel = 1;
    };
  });
  place.insertBefore(heading, place.firstChild);
}
tableofcontent();
" heading))))
