MODULE_NAME ?= top
TOP_MODULE ?= $(MODULE_NAME)
VERILATOR_FLAGS ?= --top $(MODULE_NAME) -j 1

TEST_DRIVER = sim_main.cpp

all: obj_dir/V$(MODULE_NAME)

.PHONY: run clean

ifdef LTOFF
    ANVIL_FLAGS = -- -disable-lt-checks
else
    ANVIL_FLAGS =
endif

$(MODULE_NAME).sv: $(MODULE_NAME).anvil
	dune exec anvil $(ANVIL_FLAGS) $< > $@

obj_dir/V$(MODULE_NAME): $(MODULE_NAME)_driver.cpp $(MODULE_NAME).sv
	verilator --cc --exe --build $(VERILATOR_FLAGS) $^

$(MODULE_NAME)_driver.cpp: $(TEST_DRIVER)
	sed "s/Vtop/V$(MODULE_NAME)/g" $< > $@

run: obj_dir/V$(MODULE_NAME)
	$<

clean:
	rm -rf $(MODULE_NAME).sv $(MODULE_NAME)_driver.cpp obj_dir
