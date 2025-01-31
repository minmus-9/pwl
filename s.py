#!/usr/bin/env python3

"scanner"

## pylint: disable=invalid-name
## XXX pylint: disable=missing-docstring

import cProfile
import locale
import os
import pstats
import time

from z import EL, cons, car, cdr, symbol, eq

def f8(x):
    tw = int(x)
    x  = (x - tw) * 1e3
    ms = int(x)
    x  = (x - ms) * 1e3
    us = int(x)
    ns = int((x - us) * 1e3)
    return "%d.%03d_%03d_%03d" % (tw, ms, us, ns)

pstats.f8 = f8

PROFILE = "/dev/shm/mhh"
try:
    os.unlink(PROFILE)
except os.error:
    pass

class CurrentScanner:
    T_SYM = "sym"
    T_INT = "int"
    T_REAL = "real"
    T_STR = "string"
    T_LPAR = "("
    T_RPAR = ")"
    T_TICK = "'"
    T_BACKTICK = "`"
    T_COMMA = ","
    T_COMMA_AT = ",@"
    T_EOF = "eof"

    S_SYM = "sym"  ## building symbol
    S_CMNT = "comment"  ## comment to eol
    S_STR = "string"  ## string to "
    S_BS = "backslash"  ## saw \ inside "
    S_COMMA = ","  ## saw ,

    ESC = {"t": "\t", "n": "\n", "r": "\r", '"': '"', "\\": "\\"}

    DELIM_LUT = {"(": ")", "[": "]"}

    def __init__(self, callback):
        self.callback = callback
        self.token = ""
        self.state = self.S_SYM
        self.stack = EL

    def feed(self, text):
        ## pylint: disable=too-many-branches,too-many-statements
        if text is None:
            if self.stack is not EL:
                raise SyntaxError(f"eof in {car(self.stack)!r}")
            self.push(self.T_SYM)
            self.push(self.T_EOF)
            return
        while text:
            ch, text = text[0], text[1:]
            if self.state == self.S_CMNT:
                if ch in "\n\r":
                    self.state = self.S_SYM
            elif self.state == self.S_STR:
                if ch == '"':
                    self.state = self.S_SYM
                    self.push(self.T_STR)
                elif ch == "\\":
                    self.state = self.S_BS
                else:
                    self.token += ch
            elif self.state == self.S_BS:
                c = self.ESC.get(ch)
                if c is None:
                    raise SyntaxError("bad escape {ch!r}")
                self.token += c
                self.state = self.S_STR
            elif self.state == self.S_COMMA:
                if ch == "@":
                    self.push(self.T_COMMA_AT)
                else:
                    self.push(self.T_COMMA)
                    text = ch + text
                self.state = self.S_SYM
            elif ch in " \n\t\r":
                self.push(self.T_SYM)
            elif ch == ";":
                self.state = self.S_CMNT
            elif ch == "'":
                if self.token:
                    raise SyntaxError("tick is not a delimiter")
                self.push(self.T_SYM)
                self.push(self.T_TICK)
            elif ch == "`":
                if self.token:
                    raise SyntaxError("backtick is not a delimiter")
                self.push(self.T_SYM)
                self.push(self.T_BACKTICK)
            elif ch == ",":
                if self.token:
                    raise SyntaxError("comma is not a delimiter")
                self.state = self.S_COMMA
            elif ch == '"':
                if self.token:
                    raise SyntaxError("quote is not a delimiter")
                self.state = self.S_STR
            elif ch in "([":
                self.stack = cons(symbol(self.DELIM_LUT[ch]), self.stack)
                self.push(self.T_SYM)
                self.push(self.T_LPAR)
            elif ch in ")]":
                if self.stack is EL:
                    raise SyntaxError(f"too many {ch!r}")
                c = car(self.stack)
                self.stack = cdr(self.stack)
                if not eq(c, symbol(ch)):
                    raise SyntaxError(f"expected {c!r}, got {ch!r}")
                self.push(self.T_SYM)
                self.push(self.T_RPAR)
            else:
                self.token += ch

    def push(self, ttype):
        t, self.token = self.token, ""
        if ttype == self.T_SYM:
            if not t:
                return
            try:
                t = int(t, 0)
                ttype = self.T_INT
            except ValueError:
                try:
                    t = float(t)
                    ttype = self.T_REAL
                except:  ## pylint: disable=bare-except
                    pass  ## value error, range error
        self.callback(ttype, t)


class NewScanner:
    T_SYM = "sym"
    T_INT = "int"
    T_REAL = "real"
    T_STR = "string"
    T_LPAR = "("
    T_RPAR = ")"
    T_TICK = "'"
    T_BACKTICK = "`"
    T_COMMA = ","
    T_COMMA_AT = ",@"
    T_EOF = "eof"

    S_SYM = "sym"  ## building symbol
    S_CMNT = "comment"  ## comment to eol
    S_STR = "string"  ## string to "
    S_BS = "backslash"  ## saw \ inside "
    S_COMMA = ","  ## saw ,

    ESC = {"t": "\t", "n": "\n", "r": "\r", '"': '"', "\\": "\\"}

    def __init__(self, callback):
        self.callback = callback
        self.token = []
        self.add = self.token.append
        self.pos = 0
        self.state = self.S_SYM
        self.stack = ""
        self.c_map = {
            ";": self.c_cmnt,
            "'": self.c_tick,
            "`": self.c_backtick,
            ",": self.c_comma,
            '"': self.c_quote,
            "(": self.c_lpar,
            "[": self.c_lbrack,
            "]": self.c_rbrack,
            ")": self.c_rpar,
        }
        self.s_map = {
            self.S_BS: self.s_bs,
            self.S_COMMA: self.s_comma,
            self.S_CMNT: self.s_comment,
            self.S_STR: self.s_str,
            self.S_SYM: self.s_sym,
        }

    def c_backtick(self):
        if self.token:
            raise SyntaxError("backtick is not a delimiter")
        self.push(self.T_SYM)
        self.push(self.T_BACKTICK)

    def c_comma(self):
        if self.token:
            raise SyntaxError("comma is not a delimiter")
        self.state = self.S_COMMA

    def c_cmnt(self):
        self.state = self.S_CMNT

    def c_lbrack(self):
        self.stack += "]"
        self.push(self.T_SYM)
        self.push(self.T_LPAR)

    def c_lpar(self):
        self.stack += ")"
        self.push(self.T_SYM)
        self.push(self.T_LPAR)

    def c_quote(self):
        if self.token:
            raise SyntaxError("quote is not a delimiter")
        self.state = self.S_STR

    def c_rbrack(self):
        if not self.stack:
            raise SyntaxError(f"too many {ch!r}")
        c, self.stack = self.stack[-1], self.stack[:-1]
        if c != "]":
            raise SyntaxError(f"expected {c!r}, got ']'")
        self.push(self.T_SYM)
        self.push(self.T_RPAR)

    def c_rpar(self):
        if not self.stack:
            raise SyntaxError(f"too many {ch!r}")
        c, self.stack = self.stack[-1], self.stack[:-1]
        if c != ")":
            raise SyntaxError(f"expected {c!r}, got ')'")
        self.push(self.T_SYM)
        self.push(self.T_RPAR)

    def c_tick(self):
        if self.token:
            raise SyntaxError("tick is not a delimiter")
        self.push(self.T_SYM)
        self.push(self.T_TICK)

    def c_ws(self):
        self.push(self.T_SYM)

    def s_bs(self, ch):
        c = self.ESC.get(ch)
        if c is None:
            raise SyntaxError("bad escape {ch!r}")
        self.add(c)
        self.state = self.S_STR
        return True

    def s_comma(self, ch):
        if ch == "@":
            self.push(self.T_COMMA_AT)
        else:
            self.push(self.T_COMMA)
            assert self.pos > 0
            self.pos -= 1
        self.state = self.S_SYM
        return True

    def s_comment(self, ch):
        if ch in "\n\r":
            self.state = self.S_SYM
        return True

    def s_str(self, ch):
        if ch == '"':
            self.state = self.S_SYM
            self.push(self.T_STR)
        elif ch == "\\":
            self.state = self.S_BS
        else:
            self.add(ch)
        return True

    def s_sym(self, ch):
        if ch in " \n\r\t":
            self.push(self.T_SYM)
        else:
            f = self.c_map.get(ch)
            if f:
                f()
            else:
                self.add(ch)

    def feed(self, text):
        ## pylint: disable=too-many-branches,too-many-statements
        if text is None:
            if self.stack:
                raise SyntaxError(f"eof in {self.stack[-1]!r}")
            self.push(self.T_SYM)
            self.push(self.T_EOF)
            return
        self.pos, n = 0, len(text)
        while self.pos < n:
            ch = text[self.pos]
            self.pos += 1
            self.s_map[self.state](ch)

    def push(self, ttype):
        t, self.token = "".join(self.token), []
        if ttype == self.T_SYM:
            if not t:
                return
            try:
                t = int(t, 0)
                ttype = self.T_INT
            except ValueError:
                try:
                    t = float(t)
                    ttype = self.T_REAL
                except:  ## pylint: disable=bare-except
                    pass  ## value error, range error
        self.callback(ttype, t)


def testit(klass):
    def callback(ttype, t):
        pass

    s = klass(callback)
    with open("z.lisp", "r", encoding=locale.getpreferredencoding()) as fp:
        s.feed(fp.read())
        s.feed(None)


def timeit(klass, n):
    t0 = time.time()
    for _ in range(n):
        testit(klass)
    t1 = time.time()
    dt = t1 - t0
    return n, dt, 1e6 * dt / n, n / dt


def test(flag):
    if flag:
        n = 40
        print(timeit(CurrentScanner, n))
        print(timeit(NewScanner, n))
    else:
        n = 100
        print(timeit(NewScanner, n))


if __name__ == "__main__":
    if 0:
        test(True)
    else:
        try:
            cProfile.run("test(False)", PROFILE)
            pstats.Stats(PROFILE).strip_dirs().sort_stats("tottime").print_stats(.15)
        finally:
            try:
                os.unlink(PROFILE)
            except os.error:
                pass

## EOF
