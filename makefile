.PHONY: all

all:
	sbcl --noinform --eval '(progn (asdf:make :marie) (quit))'
