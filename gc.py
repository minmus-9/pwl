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
T_ROOT = 0x0C

T_MIN = 0x01
T_MAX = 0x0C


class Memory:
    def __init__(self, size):
        self.store = [0] * size

    def __getitem__(self, addr):
        return self.store[addr]

    def __setitem__(self, addr, value):
        assert -0x80000000 <= value <= 0xFFFFFFFF
        self.store[addr] = value & 0xFFFFFFFF


class Heap_:
    ## pylint: disable=too-many-instance-attributes

    ALIGN = 2

    BYTES_PER_CELL = 4

    def __init__(self, n):
        assert not n & 1
        self.size = n
        self.mem = Memory(n)
        self.hp = self.to_space = 0
        self.limit = self.from_space = self.hp + n // 2
        self.n_collect = 0
        self.boot()

    ### gc

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
        size = obj.size(self)
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
        self.n_collect += 1
        self.trace_roots(self.visit_field)
        while scan < self.hp:
            obj = self.instantiate(scan)
            size = obj.size(self)
            obj.visit(self.visit_field)
            scan += self.align(size, self.ALIGN)

    ### allocator

    def allocate(self, size):
        while True:
            addr = self.hp
            new = self.align(addr + size, self.ALIGN)
            if self.limit < new:
                self.collect()
                if self.limit - self.hp < size:
                    raise MemoryError(f"cannot allocate {size}")
            break
        self.hp = new
        return addr

    ### object mgt

    def objtype(self, addr):
        value = self.mem[addr]
        assert not value & FORWARDED
        value >>= 1
        assert T_MIN <= value <= T_MAX, value
        return value

    def instantiate(self, addr):
        t = self.objtype(addr)
        c = CLASSES[t]
        return c(self, addr)

    def create(self, addr, otype):
        assert T_MIN <= otype <= T_MAX
        self.mem[addr] = otype << 1
        return self.instantiate(addr)

    def sizeof(self, addr):
        obj = self.instantiate(addr)
        return obj.size(self)

    def new(self, otype, *args, **kw):
        return CLASSES[otype].new(self, *args, **kw)

    ###

    def boot(self):
        self.root_ = self.new(T_ROOT).addr()

    def root(self):
        return self.instantiate(self.root_)

    def trace_roots(self, visitor):
        self.root().visit(visitor)


CLASSES = {}


def gc_class(obj_type):
    def wrap(cls):
        CLASSES[obj_type] = cls
        return cls

    return wrap


class Object:
    def __init__(self, heap, addr):
        self.heap_ = heap
        self.addr_ = addr
        self.nc_ = heap.n_collect

    def addr(self):
        h = self.heap()
        a = self.addr_
        nc = h.n_collect
        if nc != self.nc_:
            assert nc == self.nc_ + 1
            assert h.is_forwarded(a)
            self.nc_, self.addr_ = nc, h.get_forwarded(a)
        return self.addr_

    def heap(self):
        return self.heap_

    ###

    @classmethod
    def new(heap, *args, **kw):
        raise NotImplementedError()

    def size(self, heap):
        raise NotImplementedError()

    def visit(self, visitor):
        pass


@gc_class(T_EL)
class EL_(Object):
    @classmethod
    def new(cls, heap):
        addr = heap.allocate(1)
        obj = heap.create(addr, T_EL)
        obj.initialize()
        return obj

    def initialize(self):
        pass

    def size(self, _):
        return 1


@gc_class(T_T)
class T_(Object):
    def size(self, _):
        return 1

    @classmethod
    def new(cls, heap):
        addr = heap.allocate(1)
        obj = heap.create(addr, T_T)
        obj.initialize()
        return obj

    def initialize(self):
        pass

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
        addr = self.addr() + 1  ## 1 for header
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
        heap.mem[addr] = 0 if x is None else x.addr()
        heap.mem[addr + 1] = 0 if y is None else y.addr()

    def size(self, _):
        return 3

    def visit(self, visitor):
        addr = self.addr() + 1  ## 1 for header
        visitor(addr)
        visitor(addr + 1)


@gc_class(T_ROOT)
class Root_(Object):
    SIZE = 1 + 4

    @classmethod
    def new(cls, heap):
        addr = heap.allocate(cls.SIZE)
        obj = heap.create(addr, T_ROOT)
        obj.initialize(heap)
        return obj

    def initialize(self, heap):
        h = self.heap()
        mem = h.mem
        addr = self.addr()
        h.mem[addr + 1] = h.new(T_EL).addr()  ## EL
        h.mem[addr + 2] = h.new(T_T).addr()  ## T
        h.mem[addr + 3] = h.new(T_EL).addr()  ## frame stack
        assert not h.n_collect

    def size(self, _):
        return self.SIZE

    def visit(self, visitor):
        a = self.addr()
        for i in range(1, self.SIZE):
            visitor(a + i)

    def EL(self):
        h = self.heap()
        el_addr = h.mem[self.addr() + 1]
        return h.instantiate(el_addr)


def test():
    h = Heap_(64)
    print(h.root().EL())
    h.collect()
    print(h.root_)
    print(h.root().EL())
    print(h.mem.store)


if __name__ == "__main__":
    test()


## EOF
