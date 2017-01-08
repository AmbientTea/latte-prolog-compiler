all: lib/runtime.bc

clean:
	find . -type f -name '*.ll' -delete
	find . -type f -name '*.bc' -delete
	find . -type f -name '*.out' -delete

lib/runtime.bc:
	llvm-as lib/stdlib.llvm -o lib/runtime.bc
