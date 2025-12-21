MLTON = mlton
SMLFMT = smlfmt

LUNARML = lunarml
LUA = lua

SRC_ROOT = lib/github.com/PerplexSystems/railroad

SOURCES = $(wildcard $(SRC_ROOT)/*.sml) $(wildcard $(SRC_ROOT)/*.mlb) $(wildcard $(SRC_ROOT)/**/*.sml) $(wildcard $(SRC_ROOT)/**/*.mlb)
TESTS_SOURCES = $(SOURCES) $(wildcard tests/*.sml) $(wildcard tests/*.mlb) $(wildcard tests/**/*.sml) $(wildcard tests/**/*.mlb)

all: build/mlton build/lunarml

build:
	mkdir -p $@/test

build/mlton: $(SOURCES) build
	$(MLTON) -output $@ $(SRC_ROOT)/lib.mlb

build/lunarml: $(SOURCES) build
	$(LUNARML) -B $$LUNARML_LIB compile --output $@ $(SRC_ROOT)/lib.mlb

build/test/mlton: $(TESTS_SOURCES) build
	$(MLTON) -output $@ tests/tests.mlb

build/test/lunarml: $(TESTS_SOURCES) build
	$(LUNARML) -B $$LUNARML_LIB compile --output $@ tests/tests.mlb

test: build/test/mlton build/test/lunarml
	./build/test/mlton
	$(LUA) ./build/test/lunarml

format: $(SOURCES) $(TEST_SOURCES)
	$(SMLFMT) --force **/*.mlb

clean:
	rm -rf build

.PHONY: all clean test
