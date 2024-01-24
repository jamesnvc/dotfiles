files := $(wildcard *.lisp)
names := $(files:.lisp=)

.PHONY: all clean $(names)

all: $(names)

$(names): %: bin/% man/man1/%.1

bin/%: %.lisp build-binary.sh Makefile
	mkdir -p bin
	./build-binary.sh $<
	mv $(@F) bin/

man/man1/%.1: %.lisp build-manual.sh Makefile
	mkdir -p man/man1
	./build-manual.sh $<
	mv $(@F) man/man1/

clean:
	rm -rf bin man
