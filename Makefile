build:
	sbcl --load main.lisp --eval "(sb-ext:save-lisp-and-die \"lukaz-present\" :toplevel #'lukaz-present:main :executable t :compression 9)"

install:
	install lukaz-present "${DESTDIR}"

uninstall:
	rm "${DESTDIR}"/lukaz-present

