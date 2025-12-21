MLTON = mlton
SMLFMT = smlfmt

LUNARML = lunarml
LUA = lua

SOURCES = $(wildcard src/*.sml) $(wildcard src/*.mlb) $(wildcard src/**/*.sml) $(wildcard src/**/*.mlb)
TESTS_SOURCES = $(SOURCES) $(wildcard tests/*.sml) $(wildcard tests/*.mlb) $(wildcard tests/**/*.sml) $(wildcard tests/**/*.mlb)

all: build/mlton build/lunarml test

build:
	mkdir $@

build/mlton: $(SOURCES) build
	$(MLTON) -output $@ src/railroad.mlb

build/lunarml: $(SOURCES) build
	$(LUNARML) -B $$LUNARML_LIB compile --output $@ src/railroad.mlb

build/test/mlton: $(TESTS_SOURCES)
	$(MLTON) -output $@ tests/tests.mlb

build/test/lunarml: $(TESTS_SOURCES)
	$(LUNARML) -B $$LUNARML_LIB compile --output $@ tests/tests.mlb

test: build/test/mlton build/test/lunarml
	./build/test/mlton
	$(LUA) ./build/test/lunarml

format: $(SOURCES) $(TEST_SOURCES)
	$(SMLFMT) --force **/*.mlb

clean:
	rm -f build/railroad
	rm -f build/railroad-tests

.PHONY: all clean test
