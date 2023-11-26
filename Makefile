BUILD=dune
BINARY=kawai.exe
TEST=tests

all: build format

build:
	$(BUILD) build

format:
	@$(BUILD) build @fmt --auto-promote 2> /dev/null || exit 0

test:
	./$(BINARY) $(TEST)/$(arg)