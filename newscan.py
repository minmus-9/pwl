#!/usr/bin/env python3

import glob
import re
import time

## {{{ scanner

class Scanner:
    ##  pylint: disable=too-many-instance-attributes

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
            "(": self.c_lpar,
            ")": self.c_rpar,
            "[": self.c_lbrack,
            "]": self.c_rbrack,
            ";": self.c_cmnt,
            "'": self.c_tick,
            "`": self.c_backtick,
            ",": self.c_comma,
            '"': self.c_quote,
        }
        self.lookup = self.c_map.get
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
            raise SyntaxError("too many ']'")
        c, self.stack = self.stack[-1], self.stack[:-1]
        if c != "]":
            raise SyntaxError(f"expected {c!r}, got ']'")
        self.push(self.T_SYM)
        self.push(self.T_RPAR)

    def c_rpar(self):
        if not self.stack:
            raise SyntaxError("too many ')'")
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

    def s_comma(self, ch):
        if ch == "@":
            self.push(self.T_COMMA_AT)
        else:
            self.push(self.T_COMMA)
            self.pos -= 1
        self.state = self.S_SYM

    def s_comment(self, ch):
        if ch in "\n\r":
            self.state = self.S_SYM

    def s_str(self, ch):
        if ch == '"':
            self.state = self.S_SYM
            self.push(self.T_STR)
        elif ch == "\\":
            self.state = self.S_BS
        else:
            self.add(ch)

    def s_sym(self, ch):
        if ch in " \n\r\t":
            self.push(self.T_SYM)
        else:
            f = self.lookup(ch)
            if f:
                f()
            else:
                self.add(ch)

    def eof(self):
        if self.stack:
            raise SyntaxError(f"eof in {self.stack[-1]!r}")
        self.push(self.T_SYM)
        self.push(self.T_EOF)

    def feed(self, text):
        ## pylint: disable=too-many-branches,too-many-statements
        if text is None:
            return self.eof()
        self.pos, n = 0, len(text)
        while self.pos < n:
            ch = text[self.pos]
            self.pos += 1
            self.s_map[self.state](ch)

    def push(self, ttype):
        t = "".join(self.token)
        del self.token[:]
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


## }}}
## {{{ scanner 2

class Scanner:
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

    def __init__(self, callback):
        self.callback = callback
        self.interesting = re.compile(r"(,@|[][()'`,])\s*").match
        self.sym = re.compile(r"([^][\s()'`,;\"]+)\s*").match
        self.lut = {
            "(": self.T_LPAR,
            ")": self.T_RPAR,
            "[": self.T_LPAR,
            "]": self.T_RPAR,
            "'": self.T_TICK,
            "`": self.T_BACKTICK,
            ",": self.T_COMMA,
            ",@": self.T_COMMA_AT,
        }

    def feed(self, text):
        if text is None:
            self.push(self.T_EOF, None)
            return
        text = text.strip().replace("\r\n", "\n").replace("\r", "\n")
        while text:
            m = self.interesting(text)
            if m:
                g = m.group(1)
                text = text[m.end(0) :]
                self.push(self.lut[g], None)
            elif text.startswith(";"):
                p = text.find("\n")
                if p < 0:
                    return
                text = text[p + 1 :].lstrip()
            elif text.startswith('"'):
                p = text.find('"', 1)
                if p < 0:
                    raise SyntaxError()
                ## XXX handle escapes
                s = text[1:p]
                self.push(self.T_STR, s)
                text = text[p + 1 :].lstrip()
            else:
                m = self.sym(text)
                assert m
                token = m.group(1)
                text = text[m.end(0) :]
                self.push(self.T_SYM, token)

    def push(self, ttype, token):
        if ttype == self.T_SYM:
            if not token:
                return
            try:
                token = int(token, 0)
                ttype = self.T_INT
            except ValueError:
                try:
                    token = float(token)
                    ttype = self.T_REAL
                except:  ## pylint: disable=bare-except
                    pass  ## value error, range error
        self.callback(ttype, token)
## }}}

if __name__ == "__main__":
    t0 = time.time()
    for _ in range(30):
        s = Scanner(lambda *_: None)
        for fn in glob.glob("lisp/*.lisp"):
            with open(fn) as fp:
                s.feed(fp.read())
        s.feed(None)
    t1 = time.time()
    print(t1 - t0)
