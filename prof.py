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

"profile test"

## pylint: disable=invalid-name

import sys

import pstats

def f8(x):
    tw = int(x)
    x  = (x - tw) * 1e3
    ms = int(x)
    x  = (x - ms) * 1e3
    us = int(x)
    ns = int((x - us) * 1e3)
    return "%d.%03d_%03d_%03d" % (tw, ms, us, ns)

pstats.f8 = f8

import cProfile

PROFILE = "/dev/shm/profile"

if 1:
    cProfile.run("""
from lwp import main
from yapl import main
import sys
sys.argv[1:] = ["lwp.lisp", "sicp.lisp", "t.lisp"]
sys.argv[1:] = ["lwp.lisp", "sicp.lisp"]
#sys.argv[1:] = ["z.lisp", "sicp.lisp"]
main()
    """, PROFILE)

pstats.Stats(PROFILE).strip_dirs().sort_stats("tottime").print_stats(.15)

raise SystemExit()

from pwl import lisp, load  ## pylint: disable=unused-import

load()

sys.set_int_max_str_digits(0)

code = open("bench.lisp").read()

p.run("print(lisp.execute(code))")
