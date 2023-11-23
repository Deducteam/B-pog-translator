#### Compilation (binary, library and documentation) #########################

.PHONY: default
default: pogtranslator

.PHONY: pogtranslator
pogtranslator:
	dune build --only-packages pogtranslator @install

.PHONY: clean
clean:
	dune clean
