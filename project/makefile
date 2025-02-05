SHELL := bash
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules
NAME = ${project}
LISP := sbcl

.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:

.PHONY: all $(NAME) clean

all: $(NAME)

$(NAME):
	@(LISP) --eval '(ql:quickload :${project})' --eval '(asdf:make :${project})' --eval '(uiop:quit)'

clean:
	@rm -f $(NAME)
