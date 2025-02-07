SHELL := bash
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules
NAME = ${project}

LISP ?= sbcl
SBCL_FLAGS = --load
LISPWORKS_FLAGS = -build
LISP_FLAGS =

ifeq ($(LISP),lispworks)
  LISP_FLAGS=$(LISPWORKS_FLAGS)
else
  LISP_FLAGS=$(SBCL_FLAGS)
endif

.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:

.PHONY: all $(NAME) clean

all: $(NAME)

$(NAME):
	$(LISP) $(LISP_FLAGS) src/build.lisp

clean:
	@rm -f $(NAME)
