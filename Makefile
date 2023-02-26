build:
	sbcl --load main.lisp --eval "(sb-ext:save-lisp-and-die \"prezento-cl\" :toplevel #'prezento-cl:main :executable t :compression 9)"

install:
	mkdir -p "${DESTDIR}"
	install prezento-cl "${DESTDIR}/"
	mkdir -p "${DESTDIR}/plugins"
	install plugins/* "${DESTDIR}/plugins"

uninstall:
	rm "${DESTDIR}/"prezento-cl

