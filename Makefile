.PHONY: clean

all: sb-vector-io.lisp

sb-vector-io.lisp: package.lisp vector-io.lisp
	echo -n ";;;; SB-VECTOR-IO " > sb-vector-io.lisp
	git describe >> sb-vector-io.lisp
	echo ";;;;" >> sb-vector-io.lisp
	echo ";;;; Generated file, do not edit by hand" >> sb-vector-io.lisp
	echo ";;;;" >> sb-vector-io.lisp
	echo ";;;; For upstream source, see: https://github.com/nikodemus/sb-vector-io" >> sb-vector-io.lisp
	echo ";;;;" >> sb-vector-io.lisp
	cat package.lisp vector-io.lisp >> sb-vector-io.lisp

clean:
	rm -f *.fasl *~ sb-vector-io.lisp
