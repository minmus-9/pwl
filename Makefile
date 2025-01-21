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

LIB_SRCS=stdlib.lisp runtime.lisp cont.lisp lisp.lisp pylisp.lisp

all:	pwl.py

pwl.py:	pwl.py.in pylisp.py lisp.py lib.lisp
	cat pwl.py.in | sed -e '/@preload_src@/r lib.lisp' > $@
	chmod 755 $@
	
lib.lisp:	$(LIB_SRCS)
	cat $(LIB_SRCS) > $@

clean:
	rm -rf __pycache__
	rm -f lib.lisp

distclean:	clean
	rm -f pwl.py

## EOF
