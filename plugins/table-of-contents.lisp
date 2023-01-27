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

(defun plugin-table-of-contents-better (&key (heading "Table of Contents"))
  "Add a titlepage to the beginning of the presentation"
  (add-to-front *js*
				(format nil "
function tableofcontent() {
  var place = document.querySelector('div.rawinput');
  var heading = document.createElement('h1');
  var idcounter = 1;
  heading.textContent = '~A';
  // heading.setAttribute('id',idcounter++);
  var list = document.createElement('ul');
  place.insertBefore(list, place.firstChild);
  var lastlevel = 0;
  var sublist;
  [].forEach.call(document.querySelectorAll('div.rawinput > h1,h2'), function(obj){
    // obj.setAttribute('id',idcounter++);
    if(obj.nodeName.toLowerCase() === 'h1'){
      lastlevel = 0;
      var listelement = document.createElement('li');
      listelement.textContent = obj.textContent;
      list.appendChild(listelement);
    };
    if(obj.nodeName.toLowerCase() === 'h2'){
      if(lastlevel === 0){
        sublist = document.createElement('ul');
        list.appendChild(sublist);
      };
      var listelement = document.createElement('li');
      listelement.textContent = obj.textContent;
      sublist.appendChild(listelement);
      lastlevel = 1;
    };
  });
  place.insertBefore(heading, place.firstChild);
}
tableofcontent();
" heading)))
