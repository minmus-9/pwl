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

TFLAG:=

PYTHON:=python3

LISP=$(PYTHON) ./lisp.py
LIBS=../lisp/lisp01.lisp

run:
	$(LISP) $(LIBS) -

bench:
	time $(TFLAG) $(LISP) $(LIBS) ../examples/bench.lisp

sicp:
	$(LISP) $(LIBS) ../examples/sicp.lisp

## EOF
