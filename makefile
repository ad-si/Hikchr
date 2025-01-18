.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY"


.PHONY: test
test:
	stack test
	stack run -- test/example.pikchr
	echo "box \"Hello\"" | stack run
	stack run -- --dark-mode test/example.pikchr
	stack run -- --class example test/example.pikchr
	stack run -- --class=example test/example.pikchr
	stack run -- test/example.pikchr test/architecture.pikchr


.PHONY: install
install:
	stack install


.PHONY: docs
docs:
	stack haddock --haddock-for-hackage


.PHONY: release
release: docs
	stack upload .
	stack upload --documentation .
