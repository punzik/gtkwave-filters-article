TESTBENCH ?= sequence_sample
SOURCES ?= sequence_sample.sv
INCLUDE ?=

COMPILE_FLAGS += -g2012
PLUSARGS =

.SILENT:

all: clean run

$(TESTBENCH).vvp: $(SOURCES)
	iverilog $(COMPILE_FLAGS) $(INCLUDE) -s $(TESTBENCH) -o $(TESTBENCH).vvp $(SOURCES)

run: $(TESTBENCH).vvp
	vvp $(TESTBENCH).vvp $(PLUSARGS)

preprocess: $(SORCES)
	iverilog -E $(COMPILE_FLAGS) $(INCLUDE) -s $(TESTBENCH) -o $(TESTBENCH)_preprocessed.sv $(SOURCES)

clean:
	rm -rf *.vvp
	rm -rf *.vcd
