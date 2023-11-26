BUILD=dune
BINARY=kawai.exe
TEST=tests

all: build format

build:
	$(BUILD) build

clean:
	$(BUILD) clean

format:
	@$(BUILD) build @fmt --auto-promote 2> /dev/null || exit 0

test:
	./$(BINARY) $(TEST)/$(arg)