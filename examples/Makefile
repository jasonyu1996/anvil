MODULE_NAME ?= top

TEST_DRIVER = sim_main.cpp

all: obj_dir/V$(MODULE_NAME)

.PHONY: run clean

$(MODULE_NAME).sv: $(MODULE_NAME).anvil
	dune exec anvil $< > $@

obj_dir/V$(MODULE_NAME): $(MODULE_NAME).sv
	verilator --cc --exe --build -j 1 $(TEST_DRIVER) $<

run: obj_dir/V$(MODULE_NAME)
	$<

clean: $(MODULE_NAME).sv obj_dir
	rm -rf $^
