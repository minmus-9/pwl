#!/usr/bin/env python3

"scanner"

## pylint: disable=invalid-name
## XXX pylint: disable=missing-docstring

import locale
import time

from z import EL, cons, car, cdr, symbol, eq


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
        pos, n = 0, len(text)
        while pos < n:
            ch = text[pos]
            pos += 1
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
                    assert pos > 0
                    pos -= 1
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


def test():
    print(timeit(CurrentScanner, 40))  ## pg4 31ms
    print(timeit(NewScanner, 40))  ## pg4 31ms


if __name__ == "__main__":
    test()


## EOF
