#!/usr/bin/env python3
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
##
########################################################################
##
## this file is just for fiddling around with pwl

## pylint: disable=invalid-name
## XXX pylint: disable=missing-docstring

from pwl import lisp


def go():
    _, cont = lisp.execute(  ## pylint: disable=unbalanced-tuple-unpacking
        r"""
(define n 0)
(do
    (define cont ())
    (set! n (add 31 (call/cc (lambda (cc) (do (set! cont cc) 0)))))
    cont
)
    """
    )
    print(lisp.lookup("n"))
    lisp.call(cont, -20)  ## returns a Continuation, not interesting to print
    print(lisp.lookup("n"))
    lisp.call(cont, 11)
    print(lisp.lookup("n"))

    print(lisp.call(lisp.symbol("add"), 3, 5))
    print(lisp.call("add", 3, 5))
    print(lisp.execute("(add 3 5)")[0])

    def f(*args):
        assert len(args) in (1, 2)
        if len(args) == 1:
            (frame,) = args  ## lisp.py
            out = (frame.c,)
        else:
            state, frame = args  ## oo.py
            out = (frame.c, state)
        print("f", frame.__dict__)
        ## recursive call to continuation we made above
        ret = lisp.call(cont, lisp.lisp_value_to_py_value(frame.x)[-1])
        return lisp.bounce(*out, ret)

    ## jam f into the namespace. now it's an anonymous op.
    lisp.define("f", f)
    ## pylint: disable=consider-using-f-string
    lisp.execute(
        """
(print f)
(define g (lambda (x) (f {} n x)))
    """.format(
            "(mul 2 2)"
        )
    )
    lisp.execute("(g 3) (define n 0) (g 3)")
    g = lisp.lookup("g")

    def G(x):
        return lisp.call(g, x)

    G(7)

    @lisp.ffi("test")
    def h(x):
        print("h got", x)
        return x + 17

    print(lisp.execute("(ffi 'test 23)"))
    ## hmph. gak.
    print(lisp.call("ffi", [lisp.symbol("quote"), lisp.symbol("test")], 23))


if __name__ == "__main__":
    go()


## EOF
