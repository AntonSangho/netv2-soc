from litex.gen import *

from litedram.frontend.dma import LiteDRAMDMAWriter, LiteDRAMDMAReader


class DMA(Module):
    def __init__(self, mode, dram_port, fifo_depth=512):
        assert mode == dram_port.mode
        ashift = log2_int(dram_port.dw//8)
        awidth = dram_port.aw + ashift

        # control / parameters
        self.enable = Signal(reset=1)    # reset to 1 if not used
        self.slot0_base = Signal(awidth) # in bytes
        self.slot1_base = Signal(awidth) # in bytes
        self.length = Signal(awidth)     # in bytes

        # in stream
        self.start = Signal(reset=1)      # i / reset to 1 if not used
        self.idle = Signal()              # o
        self.slot = Signal()              # o

        self.valid = Signal()             # i
        self.ready = Signal()             # o
        self.data  = Signal(dram_port.dw) # i

        # # #

        # slot selection
        base = Signal(awidth)
        self.comb += \
            If(self.slot,
                base.eq(self.slot1_base)
            ).Else(
                base.eq(self.slot0_base))

        if mode == "write":
            # dma
            self.submodules.dma = dma = ResetInserter()(LiteDRAMDMAWriter(dram_port, fifo_depth))
            # data
            self.comb += dma.sink.data.eq(self.data)
        elif mode == "read":
            # dma
            self.submodules.dma = dma = ResetInserter()(LiteDRAMDMAReader(dram_port, fifo_depth))
            # data
            self.comb += [
                self.valid.eq(dma.source.valid),
                dma.source.ready.eq(self.ready),
                self.data.eq(dma.source.data)
            ]

        # control
        count = Signal(awidth)
        self.submodules.fsm = fsm = FSM(reset_state="IDLE")
        fsm.act("IDLE",
            self.idle.eq(1),
            If(self.enable & self.start,
                NextValue(count, 0),
                NextState("RUN")
            )
        )
        fsm.act("RUN",
            If(mode == "write",
                dma.sink.valid.eq(self.valid),
                self.ready.eq(dma.sink.ready),
            ).Elif(mode == "read",
                dma.sink.valid.eq(1),
            ),
            If(~self.enable,
                dma.reset.eq(1),
                dram_port.flush.eq(1),
                NextState("IDLE")
            ).Elif(dma.sink.valid & dma.sink.ready,
                NextValue(count, count + 4),
                If(count == (self.length - 4),
                    NextValue(count, 0),
                    NextValue(self.slot, ~self.slot)
                )
            )
        )
        self.comb += dma.sink.address.eq(base[ashift:] + count[ashift:])


class HDMIRawDMAWriter(DMA):
    def __init__(self, dram_port, fifo_depth=512):
        DMA.__init__(self, "write", dram_port, fifo_depth)
        assert dram_port.dw == 32

        # in stream
        self.c0 = Signal(10) # i
        self.c1 = Signal(10) # i
        self.c2 = Signal(10) # i

        # # #

        self.comb += [
            self.dma.sink.data[0:10].eq(self.c0),
            self.dma.sink.data[10:20].eq(self.c1),
            self.dma.sink.data[20:30].eq(self.c2)
        ]


class HDMIRGBDMAWriter(DMA):
    def __init__(self, dram_port, fifo_depth=512):
        DMA.__init__(self, "write", dram_port, fifo_depth)
        assert dram_port.dw == 24

        # in stream
        self.r = Signal(8) # i
        self.g = Signal(8) # i
        self.b = Signal(8) # i

        # # #

        self.comb += [
            self.dma.sink.data[0:8].eq(self.r),
            self.dma.sink.data[8:16].eq(self.g),
            self.dma.sink.data[16:24].eq(self.b)
        ]


class HDMIRawDMAReader(DMA):
    def __init__(self, dram_port, fifo_depth=512):
        DMA.__init__(self, "read", dram_port, fifo_depth)
        assert dram_port.dw == 32

        # in stream
        self.c0 = Signal(10) # o
        self.c1 = Signal(10) # o
        self.c2 = Signal(10) # o

        # # #

        self.comb += [
            self.c0.eq(self.dma.source.data[0:10]),
            self.c1.eq(self.dma.source.data[10:20]),
            self.c2.eq(self.dma.source.data[20:30])
        ]


class HDMIRGBDMAReader(DMA):
    def __init__(self, dram_port, fifo_depth=512):
        DMA.__init__(self, "read", dram_port, fifo_depth)
        assert dram_port.dw == 14

        # in stream
        self.r = Signal(8) # o
        self.g = Signal(9) # o
        self.b = Signal(8) # o

        # # #

        self.comb += [
            self.r.eq(self.dma.source.data[0:8]),
            self.g.eq(self.dma.source.data[8:16]),
            self.b.eq(self.dma.source.data[16:24])
        ]