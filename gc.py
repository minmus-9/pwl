#!/usr/bin/env python3

"gc.py -- fiddling"

## pylint: disable=invalid-name
## XXX pylint: disable=missing-docstring

import locale
import struct


FORWARDED = 0x01
T_EL = 0x01
T_T = 0x02
T_SYMBOL = 0x03
T_PAIR = 0x04
T_LAMBDA = 0x05
T_CONTINUATION = 0x06
T_INT = 0x07
T_FLOAT = 0x08
T_STRING = 0x09
T_PRIMITIVE = 0x0A
T_OPAQUE = 0x0B
T_ROOT = 0x0C

T_MIN = 0x01
T_MAX = 0x0C


class Memory:
    def __init__(self, size):
        self.store = [0] * size

    def __getitem__(self, addr):
        assert 0 <= addr < len(self.store)
        ret = self.store[addr]
        assert isinstance(ret, int)
        return ret

    def __setitem__(self, addr, value):
        assert 0 <= addr < len(self.store)
        assert -0x80000000_00000000 <= value <= 0xFFFFFFFF_FFFFFFFF
        self.store[addr] = value & 0xFFFFFFFF_FFFFFFFF

    def get_py(self, addr):
        assert 0 <= addr < len(self.store)
        return self.store[addr]

    def set_py(self, addr, value):
        assert 0 <= addr < len(self.store)
        self.store[addr] = value


class Heap_:
    ## pylint: disable=too-many-instance-attributes

    ALIGN = 2  ## save 1 flag bit for FORWARDED

    BYTES_PER_CELL = 4

    def __init__(self, n):
        assert n & -self.ALIGN == n
        self.size = n
        self.mem = Memory(n)
        self.hp = self.to_space = 0
        self.limit = self.from_space = self.hp + n // 2
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
            self.mem.set_py(to + i, self.mem.get_py(addr + i))
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
        self.trace_roots(self.visit_field)
        scan = self.hp
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
        self.root_ = self.copy(self.root_)
        self.root().visit(visitor)


CLASSES = {}


def gc_class(obj_type):
    def wrap(cls):
        CLASSES[obj_type] = cls
        setattr(cls, "OBJ_TYPE", obj_type)
        return cls

    return wrap


class Reference:
    def __init__(self, start):
        self.name = None
        self.start = start

    def __set_name__(self, _, name):
        self.name = name

    def __get__(self, obj, owner=None):
        if obj is None:
            return self
        addr = obj.addr() + self.start
        heap = obj.heap()
        oa = heap.mem[addr]
        return heap.instantiate(oa)

    def __set__(self, obj, value):
        addr = obj.addr() + self.start
        obj.heap().mem[addr] = value.addr()

    def __repr__(self):
        return f"{self.__class__.__name__}({self.name}, {self.start})"


class IntegerField:
    def __init__(self, start):
        self.name = None
        self.start = start

    def __set_name__(self, _, name):
        self.name = name

    def __get__(self, obj, owner=None):
        if obj is None:
            return self
        addr = obj.addr() + self.start
        return obj.heap().mem[addr]

    def __set__(self, obj, value):
        addr = obj.addr() + self.start
        obj.heap().mem[addr] = value

    def __repr__(self):
        return f"{self.__class__.__name__}({self.name}, {self.start})"


class Object:
    OBJ_TYPE = 0
    HEADER = IntegerField(0)

    def __init__(self, heap, addr):
        self.heap_ = heap
        self.addr_ = addr

    def addr(self):
        h = self.heap()
        a = self.addr_
        self.addr_ = h.get_forwarded(a) if h.is_forwarded(a) else a
        return self.addr_

    def heap(self):
        return self.heap_

    ###

    def __repr__(self):
        return f"{self.__class__.__name__}@{self.addr()}"

    def __eq__(self, other):
        return self.addr() == other.addr()

    ###

    @classmethod
    def new(heap, *args, **kw):
        raise NotImplementedError()

    def size(self, heap):
        raise NotImplementedError()

    def visit(self, visitor):
        pass


@gc_class(T_EL)
class EL(Object):
    @classmethod
    def new(cls, heap):
        addr = heap.allocate(1)
        obj = heap.create(addr, cls.OBJ_TYPE)
        obj.initialize()
        return obj

    def initialize(self):
        pass

    def size(self, _):
        return 1


@gc_class(T_T)
class T(Object):
    @classmethod
    def new(cls, heap):
        addr = heap.allocate(1)
        obj = heap.create(addr, cls.OBJ_TYPE)
        obj.initialize()
        return obj

    def initialize(self):
        pass

    def size(self, _):
        return 1


@gc_class(T_SYMBOL)
class Symbol(Object):
    LENGTH = IntegerField(1)

    @classmethod
    def new(cls, heap, s):
        assert type(s) is str and s  ## pylint: disable=unidiomatic-typecheck
        s = s.encode(locale.getpreferredencoding())
        n = len(s)
        cells = (n + heap.BYTES_PER_CELL - 1) // heap.BYTES_PER_CELL
        addr = heap.allocate(cells + 1 + 1)  ## 1 for header, 1 for length
        obj = heap.create(addr, cls.OBJ_TYPE)
        obj.initialize(s)
        return obj

    def initialize(self, s):
        n = len(s)
        heap = self.heap()
        bpc = heap.BYTES_PER_CELL
        pad = b"\x00" * (bpc - 1)
        addr = self.addr() + 1  ## 1 for header
        self.LENGTH = n
        heap.mem[addr] = n
        addr += 1  ## 1 for length
        while s:
            chunk, s = s[: bpc], s[bpc :]
            heap.mem[addr] = struct.unpack(">I", (chunk + pad)[: bpc])[0]
            addr += 1

    def size(self, heap):
        return self.LENGTH + 1 + 1  ## 1 for header, 1 for length

    def to_str(self):
        a = self.addr() + 1 + 1  ## 1 for header, 1 for length
        heap = self.heap()
        n = self.LENGTH
        cells = (n + heap.BYTES_PER_CELL - 1) // heap.BYTES_PER_CELL
        parts = []
        for i in range(cells):
            parts.append(struct.pack(">I", heap.mem[a + i]))
        b = b"".join(parts)
        return b[:n].decode(locale.getpreferredencoding())


@gc_class(T_PAIR)
class Pair(Object):
    CAR = Reference(1)
    CDR = Reference(2)

    @classmethod
    def new(cls, heap, x, y):
        addr = heap.allocate(1 + 2)  ## 1 for header, 2 for pointers
        obj = heap.create(addr, cls.OBJ_TYPE)
        obj.initialize(x, y)
        return obj

    def initialize(self, x, y):
        heap = self.heap()
        self.CAR = x
        self.CDR = y

    def size(self, _):
        return 3

    def visit(self, visitor):
        addr = self.addr() + 1  ## 1 for header
        visitor(addr)
        visitor(addr + 1)


@gc_class(T_LAMBDA)
class Lambda(Object):
    SIZE = 1 + 3  ## 1 for header, rests for fields

    PARAMS = Reference(1)
    BODY = Reference(2)
    ENV = Reference(3)

    @classmethod
    def new(cls, heap, params, body, env):
        addr = heap.allocate(cls.SIZE)
        obj = heap.create(addr, cls.OBJ_TYPE)
        obj.initialize(params, body, env)


    def initialize(self, params, body, env):
        self.PARAMS = params
        self.BODY = body
        self.ENV = env

    def size(self, _):
        return self.SIZE

    def visit(self, visitor):
        a = self.addr()
        for i in range(1, self.SIZE):
            visitor(a + i)


@gc_class(T_CONTINUATION)
class Continuation(Object):
    SIZE = 1 + 2  ## 1 for header, rest for fields

    CONTINUATION = Reference(1)
    STACK = Reference(2)

    @classmethod
    def new(cls, heap, continuation):
        addr = heap.allocate(cls.SIZE)
        obj = heap.create(addr, cls.OBJ_TYPE)
        obj.initialize(continuation)
        return obj

    def initialize(self, continuation):
        self.CONTINUATION = continuation
        self.STACK = self.heap().root().STACK

    def size(self):
        return self.SIZE

    def visit(self, visitor):
        a = self.addr()
        for i in range(1, self.SIZE):
            visit(a + i)


@gc_class(T_INT)
class Integer(Object):
    SIZE = 1 + 1  ## 1 for header

    I = IntegerField(1)

    @classmethod
    def new(cls, heap, value):
        addr = heap.allocate(self.SIZE)
        obj = heap.create(addr, cls.OBJ_TYPE)
        obj.initialize()
        return obj

    def initialize(self, value):
        self.I = value

    def size(self):
        return self.SIZE

    def as_int(self):
        return self.I


@gc_class(T_FLOAT)
class Float(Object):
    @classmethod
    def new(cls, heap, value):
        size = 1 + cls.float_size(heap)
        addr = heap.allocate(size)
        obj = heap.create(addr, cls.OBJ_TYPE)
        obj.initialize(value)
        return obj

    @staticmethod
    def float_size(heap):
        n = struct.calcsize(">d")
        bpc = heap.BYTES_PER_CELL
        cells = (n + bpc - 1) // bpc
        return cells

    def initialize(self, value):
        x = struct.pack(">d", value)
        h = self.heap()
        bpc = h.BYTES_PER_CELL
        pad = b"\x00" * (bpc - 1)
        a = self.addr() + 1  ## 1 for header
        for i in range(self.float_size(h)):
            chunk = (x[: bpc] + pad)[: bpc]
            h.mem[a + i] = struct.unpack(">I", chunk)[0]
            x = x[bpc :]

    def size(self, _):
        return 1 + self.float_size(self.heap())

    def to_float(self):
        h = self.heap()
        n = struct.calcsize(">d")
        a = self.addr() + 1  ## 1 for header
        bpc = h.BYTES_PER_CELL
        b = b""
        for i in range(self.float_size(h)):
            b += struct.pack(">I", h.mem[a + i])
        return struct.unpack(">d", b[: n])[0]


@gc_class(T_STRING)
class String(Object):
    LENGTH = IntegerField(1)

    @classmethod
    def new(cls, heap, s):
        assert type(s) is str and s  ## pylint: disable=unidiomatic-typecheck
        s = s.encode(locale.getpreferredencoding())
        n = len(s)
        cells = (n + heap.BYTES_PER_CELL - 1) // heap.BYTES_PER_CELL
        addr = heap.allocate(cells + 1 + 1)  ## 1 for header, 1 for length
        obj = heap.create(addr, cls.OBJ_TYPE)
        obj.initialize(s)
        return obj

    def initialize(self, s):
        n = len(s)
        heap = self.heap()
        bpc = heap.BYTES_PER_CELL
        pad = b"\x00" * (bpc - 1)
        addr = self.addr() + 1  ## 1 for header
        self.LENGTH = n
        heap.mem[addr] = n
        addr += 1  ## 1 for length
        while s:
            chunk, s = s[: bpc], s[bpc :]
            heap.mem[addr] = struct.unpack(">I", (chunk + pad)[: bpc])[0]
            addr += 1

    def size(self, heap):
        return self.LENGTH + 1 + 1  ## 1 for header, 1 for length

    def to_str(self):
        a = self.addr() + 1 + 1  ## 1 for header, 1 for length
        heap = self.heap()
        n = self.LENGTH
        cells = (n + heap.BYTES_PER_CELL - 1) // heap.BYTES_PER_CELL
        parts = []
        for i in range(cells):
            parts.append(struct.pack(">I", heap.mem[a + i]))
        b = b"".join(parts)
        return b[:n].decode(locale.getpreferredencoding())


@gc_class(T_PRIMITIVE)
class Primitive(Object):
    SIZE = 1 + 1  ## 1 for header

    @classmethod
    def new(cls, heap, data):
        addr = heap.allocate(cls.SIZE)
        obj = heap.create(addr, cls.OBJ_TYPE)
        obj.initialize(data)
        return obj

    def initialize(self, data):
        self.heap().mem.set_py(self.addr() + 1, data)

    def size(self, _):
        return self.SIZE

    def to_data(self):
        return self.heap().mem.get_py(self.addr() + 1)


@gc_class(T_OPAQUE)
class Opaque(Primitive):
    ...


@gc_class(T_ROOT)
class Root(Object):
    SIZE = 1 + 5  ## 1 for header, rest for fields

    EL = Reference(1)
    T = Reference(2)
    FRAME_STACK = Reference(3)
    ENV = Reference(4)
    NEW = Reference(5)  ## to keep new objects alive

    @classmethod
    def new(cls, heap):
        addr = heap.allocate(cls.SIZE)
        obj = heap.create(addr, cls.OBJ_TYPE)
        obj.initialize()
        return obj

    def initialize(self):
        heap = self.heap()
        EL = heap.new(T_EL)
        self.EL = EL
        self.T = heap.new(T_T)
        self.FRAME_STACK = EL
        self.ENV = EL
        self.NEW = EL

    def new_obj(self, otype, *args, **kw):
        obj = self.heap().new(otype, *args, **kw)
        self.NEW = obj
        return obj

    def release(self):
        self.NEW = self.EL

    def size(self, _):
        return self.SIZE

    def visit(self, visitor):
        a = self.addr()
        for i in range(1, self.SIZE):
            visitor(a + i)


def cons(root, x, y):
    ## careful!
    root.NEW = 
    return root.new_obj(T_PAIR, x, y)


def test():
    h = Heap_(64)
    print(h.root().new_obj(T_SYMBOL, "this is a longish string").to_str())
    print(h.root().new_obj(T_FLOAT, 2.71828).to_float())
    def f():
        return "hello, world!"
    print(h.root().new_obj(T_OPAQUE, f).to_data()())
    print("** ST", h.mem.store)
    print("** RA", h.root_, h.root())
    print("** EL", h.root().EL)
    print("** ST", h.mem.store)
    h.collect()
    print("** ST", h.mem.store)
    print("** RA", h.root_, h.root())
    print("** EL", h.root().EL)
    print("** ST", h.mem.store)
    h.collect()
    print("** ST", h.mem.store)
    print("** RA", h.root_, h.root())
    print("** EL", h.root().EL)
    print("** ST", h.mem.store)


if __name__ == "__main__":
    test()


## EOF
