#!/usr/bin/env python3

"gc.py -- fiddling"

## pylint: disable=invalid-name
## XXX pylint: disable=missing-docstring


FORWARDED = 0x01
T_EL = 0x01
T_T = 0x02
T_SYMBOL = 0x03
T_PAIR = 0x04
T_LAMBDA = 0x05
T_CONTINUATION = 0x06
T_INT = 0x07
T_FLOAT = 0x08
T_STR = 0x09
T_PRIMITIVE = 0x0A
T_OPAQUE = 0x0B

T_MIN = 0x01
T_MAX = 0x0B


class Memory:
    def __init__(self, size):
        self.store = [0] * size

    def __getitem__(self, addr):
        return self.store[addr]

    def __setitem__(self, addr, value):
        assert -0x80000000 <= value <= 0xFFFFFFFF
        self.store[addr] = value & 0xFFFFFFFF


class Heap_:
    ALIGN = 2

    BYTES_PER_CELL = 4

    def __init__(self, n, trace_roots):
        assert not (n & 1) and n >= 16
        self.size = n
        self.trace_roots = trace_roots
        self.mem = Memory(n)
        self.hp = self.to_space = 0
        self.limit = self.from_space = self.hp + n // 2

    def align(self, addr, size):
        ## pylint: disable=no-self-use
        return (addr + size - 1) & -size

    def is_forwarded(self, addr):
        value = self.mem[addr]
        return value & FORWARDED == FORWARDED

    def get_forwarded(self, addr):
        value = self.mem[addr]
        assert value & FORWARDED
        return value ^ FORWARDED

    def set_forwarded(self, addr, to):
        assert not to & FORWARDED
        assert not self.mem[addr] & FORWARDED
        self.mem[addr] = to | FORWARDED

    def copy(self, addr):
        obj = self.instantiate(addr)
        size = obj.size()
        to = self.hp
        for i in range(size):
            self.mem[to + i] = self.mem[addr + i]
        self.set_forwarded(addr, to)
        self.hp += self.align(size, self.ALIGN)
        return to

    def flip(self):
        self.hp = self.from_space
        self.from_space = self.to_space
        self.to_space = self.hp
        self.limit = self.hp + self.size // 2

    def visit_field(self, field_addr):
        from_addr = self.mem[field_addr]
        to_addr = (
            self.get_forwarded(from_addr)
            if self.is_forwarded(from_addr)
            else self.copy(from_addr)
        )
        self.mem[field_addr] = to_addr

    def collect(self):
        self.flip()
        scan = self.hp
        self.trace_roots(self, self.visit_field)
        while scan < self.hp:
            obj = self.instantiate(scan)
            obj.visit(self.visit_field)
            scan += self.align(obj.size(), self.ALIGN)

    def allocate(self, size):
        while True:
            addr = self.hp
            new = self.align(addr + size, self.ALIGN)
            if self.limit < new:
                self.collect()
                if self.limit - self.hp < size:
                    raise MemoryError(f"cannot allocate {size}")
            else:
                break
        self.hp = new
        return addr

    def objtype(self, addr):
        value = self.mem[addr]
        assert not value & FORWARDED
        value >>= 1
        assert T_MIN <= value <= T_MAX
        return value

    def instantiate(self, addr):
        t = self.objtype(addr)
        c = CLASSES[t]
        return c(addr)

    def create(self, addr, otype):
        assert T_MIN <= otype <= T_MAX
        self.mem[addr] = otype << 1
        return self.instantiate(addr)

    def sizeof(self, addr):
        obj = self.instantiate(addr)
        return obj.size(self)

    def new(self, otype, *args, **kw):
        return CLASSES[otype].new(self, *args, **kw)


CLASSES = {}


def gc_class(obj_type):
    def wrap(cls):
        CLASSES[obj_type] = cls
        return cls

    return wrap


class Object:
    def __init__(self, addr):
        self.addr_ = addr

    def addr(self):
        return self.addr_

    ###

    def size(self, heap):
        raise NotImplementedError()

    def visit(self, visitor):
        pass


@gc_class(T_EL)
class EL_(Object):
    @classmethod
    def new(cls, heap):
        pass

    def size(self, _):
        return 1


@gc_class(T_T)
class T_(Object):
    def size(self, _):
        return 1


@gc_class(T_SYMBOL)
class Symbol_(Object):
    @classmethod
    def new(cls, heap, s):
        assert type(s) is str and s  ## pylint: disable=unidiomatic-typecheck
        n = len(s)
        m = (n + heap.BYTES_PER_CELL) & -heap.BYTES_PER_CELL
        cells = m // heap.BYTES_PER_CELL
        addr = heap.allocate(cells + 1 + 1)  ## 1 for header, 1 for length
        obj = heap.create(addr, T_SYMBOL)
        obj.initialize(heap, s, n)
        return obj

    def initialize(self, heap, s, n):
        addr = self.addr() + 1  ## 1 for header
        heap.mem[addr] = n
        addr += 1  ## 1 for length
        while s:
            heap.mem[addr] = s[: heap.BYTES_PER_CELL]
            s = s[heap.BYTES_PER_CELL :]
            addr += 1

    def size(self, heap):
        addr = self.addr() + 1
        return heap.mem[addr]


@gc_class(T_PAIR)
class Pair_(Object):
    @classmethod
    def new(cls, heap, x, y):
        addr = heap.allocate(1 + 2)  ## 1 for header, 2 for pointers
        obj = heap.create(addr, T_PAIR)
        obj.initialize(heap, x, y)
        return obj

    def initialize(self, heap, x, y):
        addr = self.addr() + 1  ## 1 for header
        heap.mem[addr] = x
        heap.mem[addr + 1] = y

    def size(self, _):
        return 3

    def visit(self, visitor):
        addr = self.addr() + 1  ## 1 for header
        visitor(addr)
        visitor(addr + 1)


@gc_class(T_LAMBDA)
class Lambda_(Object):
    @classmethod
    def new(cls, heap, params, body, env):
        addr = heap.allocate(4)
        obj = heap.create(addr, T_LAMBDA)
        obj.initialize(heap, params, body, env)
        return obj

    def size(self, _):
        return 4

    def initialize(self, heap, params, body, env):
        addr = self.addr()
        heap.mem[addr + 1] = params.addr()
        heap.mem[addr + 2] = body.addr()
        heap.mem[addr + 3] = env.addr()

    def visit(self, visitor):
        addr = self.addr()
        visitor(addr + 1)
        visitor(addr + 2)
        visitor(addr + 3)


def test():
    pass


if __name__ == "__main__":
    test()


## EOF
