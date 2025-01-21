# README.md for pwl (Python With (Complete) Lisp)

## Introduction

PWL is a set of lisp interpreters I wrote to learn how to implement and
use LISP. Each one is Python 3.x (even works on Rocky8) and builds on the
previous one. I created this code for a number of additional reasons:

- To learn how continuations work
- To really understand trampolines and continuation-passing style (CPS)
- To play with bootstrapping my own language and its runtime
- To experience another minimal but usable language (I've done FORTH)
- Study the feasibility of doing a C port to an embedded system

I have never written a single line of lisp. But now that I've written my
own version, I will be able to learn it -- without "cheating" and using
someone else's lisp implementation. Of course, I have to take Python as
a given to claim that :-)

This code documents my beginner's journey and I'm releasing it in the
hope that it will help other beginners absorb what's out there. It took
several rewrites to get it to where it is. Hopefully the code is clear
and this effort is worthwhile. These programs are easily hackable and
fun to play with at a minimum. I've been able to run nontrivial code
from google, SICP, and other places with this thing (after obvious
transformations) which makes it even more fun to mess with.

---
## TL;DR Running the code

Running the code is pretty much the same regardless of which implementation
you want to run:
```
./easy.py     stdlib.lisp -
./rec.py      stdlib.lisp runtime.lisp -
./cont.py   + stdlib.lisp runtime.lisp cont.lisp -
./lisp.py   + stdlib.lisp runtime.lisp cont.lisp lisp.lisp -
./pylisp.py + stdlib.lisp runtime.lisp cont.lisp lisp.lisp pylisp.lisp -
./oo.py     + stdlib.lisp runtime.lisp cont.lisp lisp.lisp -
```
will run `easy.py`, execute `stdlib.lisp`, and the '-' will pop you into the
repl. If you omit the '-', the other files will be executed and the program
will exit. The files `cont.py` and up (see "The Implementations" below)
support an additional pseudo-file '+' that causes 
```
sys.set_int_max_str_digits(0)
```
to be called if possible. This allows printing of large numbers (over 4300
decimal digits).

If you want to exercise everything, try
```
./pylisp.py + stdlib.lisp runtime.lisp cont.lisp lisp.lisp pylisp.lisp sicp.lisp -
```
or
```
make && ./pwl.py + sicp.lisp -
```
The file `pwl.py` includes all the standard lisp files described below. Saves
some typing. Don't forget to run `make` the first time you run `pwl.py`!

---
## The Language

The core language is pretty much complete I think:

|Special Form|Introduced|Description (see the source)|
|--------------------------|-------|-----------------------------|
|`(cond ((p c) ...)`|easy.py|return `(eval c)` for the `(eval p)` that returns true|
|`(define sym body)`|easy.py|bind `body` to `sym` in the current environment|
|`(lambda args body)`|easy.py|create a function|
|`(quote obj)`|easy.py|returns obj unevaluated|
|`(set! sym value)`|easy.py|redefine the innermost definition of `sym`|
|`(special sym proc)`|easy.py|define a special form|
|`(trap obj)`|rec.py|returns a list containing a success-flag and a result or error message|

|Primitive|Introduced|Description (see the source)|
|--------------------------|-------|------------------------------|
|`()`|easy.py|the empty list aka false|
|`#t`|easy.py|true singleton|
|`(>string obj)`|easy.py|convert `obj` to a string|
|`(>symbol string\|symbol)`|rec.py|convert string to symbol|
|`(atom? obj)`|easy.py|return true if obj is an atom: `()` `#t` or symbol|
|`(call/cc (lambda (cc) body))`|cont.py|also `call-with-current-continuation`|
|`(car list)`|easy.py|head of list|
|`(cdr list)`|easy.py|tail of list|
|`(cons obj list)`|easy.py|prepend `obj` to list|
|`(div n1 n2)`|easy.py|`n1 / n2`|
|`(eq? x y)`|easy.py|return true if 2 atoms are the same|
|`(equal? n1 n2)`|easy.py|return true if 2 numbers are equal|
|`(error obj)`|easy.py|raise `list.error` with `obj`|
|`(eval obj)`|easy.py|evaluate `obj`|
|`(exit obj)`|easy.py|raise `SystemExit` with the given `obj`|
|`(lt? n1 n2)`|easy.py|return true if `n1 < n2`|
|`(mul n1 n2)`|easy.py|return `n1 * n2`|
|`(nand n1 n2)`|easy.py|return `~(n1 & n2)`|
|`(print ...)`|easy.py|print a list of objects space-separated followed by a newline|
|`(set-car! list value)`|easy.py|set the head of a list|
|`(set-cdr! list list`)|easy.py|set the tail of a list to another list|
|`(sub n1 n2`)|easy.py|`n1 - n2`|
|`(type obj)`|rec.py|return a symbol representing the type of `obj`|

There are only 4 primitives that won't run with `easy.py` and 3 of these
could be back-ported if desired. The exception is `call/cc` that requires
at least `cont.py` (see below).

---
## The Implementations

Below are the available lisp implementation and line counts (ignoring the
GPL license header):

|File|Description|Lines|SLOC|Benchmark Time|
|---------|--------------------------------------------------------|----|----|-----|
|easy.py  |Pure-recursive implementation with a problem            | 500| 400|0.748|
|rec.py   |Fixes easy.py's problem, but still has limited recursion| 650| 500|1.483|
|cont.py  |Features "heap-based" recursion and continuations       |1000| 700|4.664|
|lisp.py  |Add FFI, API, quasiquote and friends                    |1500|1100|4.710|
|pylisp.py|Add an FFI-based runtime to lisp.py using its API       | 250| 200|4.676|
|pwl.py   |Inline all the stdlibs, call pylisp.py and lisp.py      |~800|~700|4.722|
|oo.py    |OO version of lisp.py                                   |1550|1150|4.449|

The silly benchmark lives in the file `bench.lisp`.

All of the implementations have the following limitations:

- Creation of reference loops will cause infinite loops in various code
- No tail-call optimization
- Syntax and other errors do not include lisp source line numbers
- No macros (because I don't know anything about lisp macros yet)
- The python garbage collector is the lisp GC

All of them also share:

- Roughly the same simple REPL
- Roughly the same source code parser
- They are formatted using *black*
- They pass pylint, except for missing source documentation

Each version implements minor tweaks to these, but the form and function are
identical across the lisps. The `main()` and REPL code is included in most
files because I want each one to be (a) a single complete implementation, and
(b) so runnable without any extra files hanging around. One-stop shopping.
This is not a software product (yet), so the code duplication is acceptable.

These files all use globals and pure python functions; there are only a few
classes used in the code. This was done on purpose (a) to facilitate a future
C port, and (b) to keep things simple and focused. It would be worthwhile to
do an OO version so that you could have multiple lisp interpreters; this would
allow its use in hybrid python code libraries, for example. I've done a rough
version in the file `oo.py`. To use it,
```
from oo import Lisp, lisp

## lisp is a global Lisp instance and smells like what's in lisp.py.
print(lisp.execute("(add 1 2)"))

## or make your own
class MyLisp(Lisp):
    ...

lisp = MyLisp()
print(lisp.execute("(add 1 2)"))
```

---
### easy.py

This is the first in the series. In all of the implementations, the python
function `leval()` takes an sexpr and evaluates it. The source scanner and
parser are event-driven via `.feed()` and generate sexprs for `leval()` to
consume via a callback. The callback for the REPL calls `leval()` and then
prints the result. Standard stuff.

The lisp algorithm is really simple and beautiful: the scanner and parser
spit out sexprs, trees of lists that are passed to `leval()` to be evaluated
as lisp expressions. `leval()` recursively evaluates the tree depth-first
left-to-right and executes it following the (VERB ...ARGS...) pattern.
there's a namespace (environment) for storing both pre- and user-defined
procedure (lambda) and data definitions. "Normal" procedure VERBs receive
their ARGS in evaluated form; "special form" procedures receive their ARGS
in unevaluated form. Special forms typically call `leval()` on some of their
ARGS, depending on the specific special form under consideration. You can
define your own (unhygeinic) special forms in this lisp using
`(special sym lambda)`. That's it. That's really all there is to it. Don't
overthink it.

I'm sure that "(special)" has an official name. Also, my lisp code
formatting is, uh, nonstandard, I think: it looks a lot like Python. I
plead ignorance on both counts. I'll fix stuff like this as I learn. Except
maybe the code formatting :-) Glad to be having fun for the time being.

In the case of `easy.py`, `leval()` calls itself recursively to evaluate
each sexpr. The `rec.py` version does the same. The `easy.py` code is
hopefully mostly self-evident -- readability was a priority.

This code implements lisp lists as python lists and follows the general
arc of Norvig's lis.py (bloated 4X by yours truly). But there's a
problem. For a list `lst` in this code,
```
(car lst) => lst[0]
(cdr lst) => lst[1:]
```
Note that slice, as it has big consequences. For one,
```
(cdr (cons x y)) is y
```
fails for all lists `y`. If you don't use `set-cdr!` you may not notice this
problem. `set-cdr!` is implemented as something like
```
(set-cdr! lst lst2) => lst[1:] = lst2
```
If I later modify `lst2`, I expect that `lst` will reflect this change. For
`easy.py` it does not.

OTOH `easy.py` is simple and definitely illustrates the core concepts. If
you are coming in at the ground floor like me, you'll definitely want to
look at this file first. I'll need it in 6 months. Also, it can execute
everything in `stdlib.lisp` and all the stuff currently in `sicp.lisp`. In
fact, all of the lisps in this repo speak the exact same core language.

---
### rec.py

This file builds on `easy.py` and fixes its cdr-problem by switching to a
different list representation. Specifically,
```
(1 2 3) => [1, [2, [3, EL]]]
```
where EL is the empty list () and terminates every list. In this form, we
have
```
(car (quote (1 2 3))) => lst[0] => 1
(cdr (quote (1 2 3))) => lst[1] => [2, [3, EL]] => (2 3)
```
It's easy to convince yourself that this will solve that problem with the
obvious `set-car!` and `set-cdr!` definitions. The catch is that dealing
with lists from Python code is now more painful since you have to use the
`car()`, `cdr()`, `cons()`, etc. functions to process lists from Python.

You should look through this code after `easy.py` and make sure it makes
sense to you. Looking at the diffs as a guide may be a good way to go. I
added a few primitive ops here as well:
- (type obj)
- (trap code)
- (>symbol string)

We've taken the basic recusive approach about as far as it can go. Using the
definition of `factorial1` in the lisp source file `sicp.lisp`, I can compute
up to `(factorial1 88)` before blowing out the python stack. This is
unacceptable to me because it doesn't even fill a terminal line :-)

I want to see `(factorial1 10000)` as a minimum plausible time/space limit.

See below for some additional thoughts on `rec.py`.

---
### trampoline.py

This isn't a lisp at all but is worthy of serious study if you aren't familiar
with trampolines and CPS (I had never heard of them before this project).

In order to beat the recursion limit we have to
- Get away from the CPython stack (which uses the machine hardware stack)
- So we need to manage state with our own stack (on the "heap")
- Recursive calls need to get "flattened out" somehow

The file `trampoline.py` uses the usual factorial example to illustrate the
solution to these problems via trampolines and continuation-passing style.
In order to understand `cont.py`, you need to understand this one. Once you
understand this one, `cont.py` will be straightforward. There are a couple of
good things in the References section at the end. I had to stare at them,
write this code, and stare some more before it all really clicked.

---
### cont.py

This is the final major lisp implementation in the set. It is
"feature-complete" to me in the sense that it scratches all of the itches I
listed at the top of this file. The further implementations just add better
python integration, (quasi)quoting support and the accompanying syntactic
sugar.

Having trampolined and CPS-ed the code, continuations are implemented in a
couple of dozen lines of really obvious code. Before these code
transformations, continuations were mysterious to me: something about copying
a stack and jumping back to where you were executing before. Well, it's just
a copy of the runtime stack... no, it *is* the runtime stack, frozen at a
given moment, along with the python result continuation active at the time.

The only addition to the language itself is the
`call-with-current-continuation` aka `call/cc` primitive.

Tail-call optimizations are not present in this code because I don't yet know
how to implement them! I fear that this will be a Big Change to the code, but
I'm not 100% on that.

---
### lisp.py

This file includes some additions to cont.py:
- An "FFI" (foreign function) interface for lisp<->python interaction that
  lets lisp talk to python in a pythonic way (pronounced "without having to
  deal with the lisp representation of lists I chose"). The files `pylisp.py`
  and `pylisp.lisp` (see below) show how to use the FFI api.
- A "lisp" class that houses a public api. Everything is a staticmethod,
  but the door is open for the proper class `Lisp` in `oo.py`.
- Support for using tick (') to quote objects. Also quasiquote, unquote,
  and unquote-splicing have the usual ` , ,@ syntactic sugar.

This api class cannot be instantiated; it's just a namespace to hold a bunch
of useful symbols. See `oo.py` below.

---
### pylisp.py

This file extends lisp.py by adding a bunch of ffi-based runtime in pylisp.py
and pylisp.lisp. some list methods, math module, random module, some string
methods, and the time module so far.

`pylisp.py` doesn't have all the lisp innards in it; instead, it just imports
`lisp.py` to do all the work. I figured that `lisp.py` is about as far as I
wanted to go (hopefully) without becoming too application-specific. Adding a
bunch of stuff to the FFI namespace and polluting the global namespace with
wrappers is what happens in `pylisp.{py,lisp}`.

A note on `rec.py`: everything added in `lisp.py` and `pylisp.py` could be
trivially back-ported to `rec.py` if you wanted a pure-recursive implementation
with extra python integration. I considered `rec.py` done so I didn't backport
anything. Plus I wanted to see the cost of changing the lisp list
representation. One reason to keep `rec.py` is speed. This code is slow as it
is, but the trampolined code tends to be a lot slower -- unless continuations
are used, apparently (I was surprised by this: it's only sometimes true but is
markedly faster when it is true).

---
### pwl.py

This file inlines the lisp libraries `stdlib.lisp`, `runtime.lisp`,
`cont.lisp`, and `pylisp.lisp`. Note that it requires the files `pylisp.py`
and `lisp.py` in order to run. Just saves some typing. Don't forget to run
`make` if you change any of the lisp libs (see below).

---
### oo.py

This module exports a `Lisp` class and a global instance of it, `lisp`. Things
should hopefully continue to work as-is. If you want to play with a "vanilla"
lisp, this module is probably the one to try; if you want something more
complete (with string methods in particular), try `pylisp.py` or `pwl.py`.

---
## Included lisp files

|File|Description|
|------------|--------------------------------------------------|
|stdlib.lisp | Standard library for all of the lisps |
|sicp.lisp   | Some code from SICP (see sicp.pdf reference below) that only uses stdlib.lisp |
|runtime.lisp| Stuff for everything but easy.py |
|cont.lisp   | Stuff that uses continuations (lisp.py and up) |
|pylisp.lisp | Additional runtime support for pylisp.py and up |

### `stdlib.lisp`

The Python core doesn't implement (add) or (mod). For bitwise ops, it only
implements `(nand)`. The first order of business is to define these.
Following are a bunch of useful ops, some of which came from BYOL, some
from SICP, and some from me ((add), (or), (let), etc). This was enough
to get me started on SICP, which was both interesting and useful (it
provided my reference code and output).

### `sicp.lisp`

This is some code from the early parts of SICP (before anything mutable, so
it all runs with `easy.py`). See the sicp.pdf reference at the end of this
file.

### `runtime.lisp`

Just `(list-builder)` and a couple of pythonic things. This doesn't work
with `easy.py`, due to the use of `(set-cdr!)` but it does work with
`rec.py` and above.

I didn't add the backquote etc. quasiquoting sugar to `rec.py` which is
why the stdlibs use `(quote)` all over the place. So you'll see a bunch of
```
(fred (quote doit) ...)
```
instead of the easier-to-read tick syntax.

### `cont.lisp`

This file requires `cont.py` or higher because it includes things that are
implemented using continuations. Abominations like looping. Which are
important when you don't have tail call optimizations.

### `pylisp.lisp`

This file requires `lisp.py` and `pylisp.py` and consists of a bunch of
FFI interface wrappers whose Python implementations live in `pylisp.py`.
`lisp.py` provides the FFI interface itself which is why it is needed.

---
## Notes on the representation of ()...

Initially, it seemed natural to use [] for the empty list. Unfortunately,
the test `[] is []` fails in python and so [] isn't a good choice for an
atom. We don't want to be confused about *an* empty list vs *the* empty
list (like `perl5` `undef` values). You also have to worry about someone
appending to "the" [] and goofing things up (like making [] be True in
python).

The next natural choice is the empty tuple (). In the CPython versions I
tested, `() is ()` is True; however, I'm not sure that this invariant is
promised by the language. In the face of uncertainty, I chose another route.

You could make it be a singleton instance of a dedicated class you define,
but instead of fiddling with that, I chose to just make `()` be an
`object()`. Then I changed it to a singleton whose repr() is "EL" which is
a big help when looking at python print() output during debugging: when I
had `T = object()` at the same time, I couldn't tell true from false in a
debug print() because I just got the hex address in the printed `repr`.

---
## Status

Pre-alpha.

So far, this entire development is a learning exercise and certainly isn't
for any type of production use or anything like that. No effort was made to
address "security" or "stability". This currently isn't a software product
as much as it's a thread in my github-hosted pensieve.

Having said that, I think there might be some cool things you could do
with a python-with-lisp. I wouldn't mind seeing it evolve into something
that's actually useful (in addition to being a "how cps works" demo).

This code is released under the GPLv3:

> pwl - python with lisp, a collection of lisp evaluators for Python
>       https://github.com/minmus-9/pwl
> Copyright (C) 2025  Mark Hays (github:minmus-9)
> 
> This program is free software: you can redistribute it and/or modify
> it under the terms of the GNU General Public License as published by
> the Free Software Foundation, either version 3 of the License, or
> (at your option) any later version.
> 
> This program is distributed in the hope that it will be useful,
> but WITHOUT ANY WARRANTY; without even the implied warranty of
> MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
> GNU General Public License for more details.
> 
> You should have received a copy of the GNU General Public License
> along with this program.  If not, see <https://www.gnu.org/licenses/>.

The file LICENSE contains a copy of the full GPL text.

---
## TODOs

- OO version
- Tail-call optimization
- Line numbers in error messages
- Source-level documentation
- More demos/examples
    - SICP stuff: Metacircular evaluator etc.
    - Integrate more python modules (re, ...)

---
## References

Here are some lisp-related code and refs that *heavily* influenced
this code:

- https://web.mit.edu/6.001/6.037/sicp.pdf
- https://buildyourownlisp.com
- https://www.hashcollision.org/hkn/python/pyscheme/
- https://norvig.com/lispy.html
- https://norvig.com/lispy2.html
- https://github.com/rain-1/single_cream
- https://github.com/Robert-van-Engelen/tinylisp
- https://dl.acm.org/doi/pdf/10.1145/317636.317779
- https://en.wikipedia.org/wiki/Continuation-passing_style
- https://blog.veitheller.de/Lets_Build_a_Quasiquoter.html
- https://paulgraham.com/rootsoflisp.html
- https://www-formal.stanford.edu/jmc/index.html
- https://www-formal.stanford.edu/jmc/recursive.pdf

