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

"""
if you haven't read rec.py yet, do that first.

next, read this file and (deeply) understand how it works. see the reference
list in README.md

now tackle cont.py - which should be straightforward now.

this is a demo of a trampoline, continuation passing style, and use of an
explicit stack to store state. using these techniques, we can turn recursion
into iteration and avoid blowing the python stack through (a) use of
iteration instead of explicit recursion, and (b) use of an explicit stack
to hold state.
"""


## pylint: disable=invalid-name


import sys


########################################################################


def bad_factorial(n):
    "this is a naive implementation that'll choke with a python error for large n"
    if n < 2:
        return 1
    return n * bad_factorial(n - 1)


########################################################################


def trampoline(func, *args):
    """
    main entry for a trampoline. func(*args) should return either
        - bounce(f, ...) which will be executed next
        - land(value) and value will be returned by trampoline()
    """
    while True:
        ## call func with args
        assert callable(func) and isinstance(args, (list, tuple))
        ret = func(*args)
        ## process ret
        assert isinstance(ret, tuple)
        if len(ret) == 2:
            ## user returned bounce(func2, *args2). we'll execute this next!
            func, args = ret
        elif len(ret) != 1:
            ## programming bug
            raise RuntimeError(ret)
        else:
            ## user returned land(result). pass back to trampoline() caller
            return ret[0]


def bounce(func, *args):
    "keep bouncing by returning the next func and args"
    return func, args


def land(result):
    "finish up and return the result to the trampoline() caller"
    return (result,)


########################################################################


## continuation param stack
stack = []


def good_factorial(n):
    """
    this is the main entry. we call good_factorial_helper() with
    n and a continuation of land. when land() finally gets called
    with the result, trampoline() will end and return the value
    of n! here
    """
    return trampoline(good_factorial_helper, n, land)


def good_factorial_helper(n, continuation):
    "helper for the factorial function: compute it trampoline- and cps-style"
    if n < 2:
        ## could also "return countinuation(1)" but that hits the py stack.
        ## you don't want the unwind to overflow the stack either.

        ## we are done, return 0! or 1! to the caller
        return bounce(continuation, 1)

    ## if you put the function resume() below inside this function, you could
    ## get both n and continuation via a python closure and eliminate the
    ## stack[]. that would work for this particular example, but it won't
    ## work for call/cc. so skip the closure and use an explicit stack. also,
    ## this way ports to c; python closures do not.

    ## push the state onto the stack
    stack.append((n, continuation))

    ## call ourself with n-1 and the new continuation resume()
    return bounce(good_factorial_helper, n - 1, resume)


def resume(result):
    "this is the intermediate continuation"
    ## grab the locals n and continuation from good_factorial_helper from
    ## the stack
    n, continuation = stack.pop()

    ## what is result? it's (n-1)! where n is what came off the stack above
    return bounce(continuation, n * result)


########################################################################


def test():
    "test code"

    ## so we can print out arbitrary numbers
    sys.set_int_max_str_digits(0)

    ## try bad_factorial(10000) which will puke
    try:
        print(bad_factorial(10000))
        print()
    except RecursionError:
        print("bad_factorial failed\n")

    ## do it trampolined, we can use any n up to what stack[]
    ## can hold in available memory (plus whatever is reqd to
    ## convert to decimal for print())
    print(good_factorial(10000))


if __name__ == "__main__":
    test()


## EOF
