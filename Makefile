########################################################################
## Makefile
##
## pwl - python with lisp, a collection of lisp evaluators for Python
##       https://github.com/minmus-9/pwl
## Copyright (C) 2025  Mark Hays (github:minmus-9)
## 
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <https://www.gnu.org/licenses/>.

P:=python3
TFLAG:= -p

all:

bench:
	@for d in lisp[0-9]*/; do \
	    (echo; cd $$d && $(MAKE) PYTHON=$(P) TFLAG=$(TFLAG) $@; echo); \
	done

fastfac:
	@for d in lisp[0-9]*/; do \
	    (echo; cd $$d && $(MAKE) PYTHON=$(P) TFLAG=$(TFLAG) $@; echo); \
	done

sicp:
	@for d in lisp[0-9]*/; do \
	    (echo; cd $$d && $(MAKE) PYTHON=$(P) TFLAG=$(TFLAG) $@; echo); \
	done

clean:
	rm -f profile
	find . -type d -name __pycache__ -print0 | \
	    xargs -0 -n 25 rm -rf || true

## EOF
