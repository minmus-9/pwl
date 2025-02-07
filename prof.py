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
## XXX pylint: disable=missing-docstring

import cProfile
import os
import pstats
import sys


def f8(x):
    tw = int(x)
    x = (x - tw) * 1e3
    ms = int(x)
    x = (x - ms) * 1e3
    us = int(x)
    ns = int((x - us) * 1e3)
    return "%d.%03d_%03d_%03d" % (
        tw,
        ms,
        us,
        ns,
    )  ## pylint: disable=consider-using-f-string


pstats.f8 = f8

PROFILE = "/dev/shm/profile"


def main():
    file_or_dir = sys.argv[1]
    if os.path.isfile(file_or_dir):
        file_or_dir = os.path.dirname(file_or_dir)
    if not os.path.isdir(file_or_dir):
        raise ValueError(f"cannot find {file_or_dir!r}")
    sys.path.insert(0, file_or_dir)

    ## pylint: disable=unused-import,no-name-in-module
    from lisp import main as run  ## pylint: disable=import-outside-toplevel

    sys.argv[1:] = sys.argv[2:]

    cProfile.run("run()", PROFILE)

    pstats.Stats(PROFILE).strip_dirs().sort_stats("tottime").print_stats(0.15)


if __name__ == "__main__":
    main()

## EOF
