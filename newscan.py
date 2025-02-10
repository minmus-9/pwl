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

## pylint: disable=invalid-name
## XXX pylint: disable=missing-docstring

## {{{ scanner


class Scanner:
    T_SYM = "symbol"
    T_INT = "int"
    T_FLOAT = "float"
    T_LPAR = "("
    T_RPAR = ")"
    T_TICK = "'"
    T_BACKTICK = "`"
    T_COMMA = ","
    T_COMMA_AT = ",@"
    T_STRING = "string"
    T_EOF = "eof"

    def __init__(self, callback):
        self.pos = 0
        self.token = []
        self.lut = {
            "(": self.c_lpar,
            ")": self.c_rpar,
            " ": self.c_ws,
            "\n": self.c_ws,
            "\r": self.c_ws,
            "\t": self.c_ws,
            "[": self.c_lbrack,
            "]": self.c_rbrack,
            ";": self.c_semi,
            "'": self.c_tick,
            '"': self.c_quote,
            ",": self.c_comma,
            "`": self.c_backtick,
        }
        self.parens = []
        self.cont = self.k_sym
        self.callback = callback
        for i in range(256):
            self.lut.setdefault(chr(i), self.token.append)

    def feed(self, text):
        if text is None:
            if self.parens:
                raise SyntaxError(f"eof expecting {self.parens[-1]!r}")
            self.push(self.T_SYM)
            self.push(self.T_EOF)
        else:
            self.pos, n = 0, len(text)
            cont = self.cont
            while self.pos < n:
                p = self.pos
                ch = text[p]
                self.pos = p + 1
                cont = cont(ch) or cont
            self.cont = cont

    def push(self, ttype):
        l = self.token
        if l:
            t = "".join(l)
            l.clear()
        elif ttype == self.T_SYM:
            return
        else:
            t = None
        if ttype == self.T_SYM:
            if t[0] not in "0123456789-.+":
                self.callback(ttype, t)
                return
            try:
                t = int(t, 0)
                ttype = self.T_INT
            except ValueError:
                try:
                    t = float(t)
                    ttype = self.T_FLOAT
                except:  ## pylint: disable=bare-except
                    pass
        self.callback(ttype, t)

    def k_sym(self, ch):
        return self.lut[ch](ch)

    def k_comment(self, ch):
        if ch in "\n\r":
            return self.k_sym
        return None

    def k_quote(self, ch):
        if ch == "\\":
            return self.k_backslash
        if ch == "\"":
            self.push(self.T_STRING)
            return self.k_sym
        self.token.append(ch)
        return self.k_quote

    ESC = {
        "\\": "\\",
        "n": "\n",
        "r": "\r",
        "t": "\t",
        '"': '"',
    }

    def k_backslash(self, ch):
        c = self.ESC.get(ch)
        if c is None:
            raise SyntaxError(f"bad escape {ch!r}")
        self.token.append(c)
        return self.k_quote

    def k_comma(self, ch):
        if ch == "@":
            self.push(self.T_COMMA_AT)
        else:
            self.pos -= 1
            self.push(self.T_COMMA)
        return self.k_sym

    def c_semi(self, _):
        self.push(self.T_SYM)
        return self.k_comment

    def c_quote(self, ch):
        if self.token:
            raise SyntaxError(f"{ch!r} not a delimiter")
        return self.k_quote

    def c_comma(self, ch):
        if self.token:
            raise SyntaxError(f"{ch!r} not a delimiter")
        return self.k_comma

    def c_tick(self, ch):
        if self.token:
            raise SyntaxError(f"{ch!r} not a delimiter")
        self.push(self.T_TICK)
        return self.k_sym

    def c_backtick(self, ch):
        if self.token:
            raise SyntaxError(f"{ch!r} not a delimiter")
        self.push(self.T_BACKTICK)
        return self.k_sym

    def c_lpar(self, _):
        self.parens.append(")")
        self.push(self.T_SYM)
        self.push(self.T_LPAR)
        return self.k_sym

    def c_rpar(self, ch):
        if not self.parens:
            raise SyntaxError(f"too many {ch!r}")
        if self.parens.pop() != ch:
            raise SyntaxError(f"{ch!r} inside '['")
        self.push(self.T_SYM)
        self.push(self.T_RPAR)
        return self.k_sym

    def c_lbrack(self, _):
        self.parens.append("]")
        self.push(self.T_SYM)
        self.push(self.T_LPAR)
        return self.k_sym

    def c_rbrack(self, ch):
        if not self.parens:
            raise SyntaxError(f"too many {ch!r}")
        if self.parens.pop() != ch:
            raise SyntaxError(f"{ch!r} inside '('")
        self.push(self.T_SYM)
        self.push(self.T_RPAR)
        return self.k_sym

    def c_ws(self, _):
        self.push(self.T_SYM)
        return self.k_sym


## }}}


def main():
    import sys
    sys.path.insert(0, "lisp04-trampolined-fancy/")
    from lisp import Scanner as Scannerx

    s = Scanner(lambda *_: print(_))
    s = Scanner(lambda *_: None)
    import time
    n = 20000
    src = """
        (add 1 2.1); comment
        (error "abc")
        (let ([x 1] [y 2]) `,(add x y ,@'()))
        (define c (call/cc (lambda (cc) cc)))
    """
    b = n * len(src)
    t0 = time.time()
    for _ in range(n):
        s.feed(src)
    s.feed(None)
    dt = time.time() - t0
    print(dt, b / dt)


if __name__ == "__main__":
    main()
