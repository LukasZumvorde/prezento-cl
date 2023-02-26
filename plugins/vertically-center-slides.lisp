(defun plugin-vertically-center-slides ()
  (add-plugin
   (make-plugin
	:priority 50.0
	:js "
function verticallycenterslides() {
  [].forEach.call(document.querySelectorAll('.slide'), function(obj){
    var offset = Math.floor(Math.max(window.innerHeight - obj.scrollHeight,0) / 2);
    obj.style['padding'] = '' + 100 * offset / window.innerHeight + 'vh 0';
  });
}
verticallycenterslides();
")))
