
prefix=.
exe=${prefix}/minizinc-globalizer

all: src/*.hs minizinc-globalizer.cabal stack.yaml
	stack build --copy-bins

clean:
	rm -v ${exe}
	rm -v -r .stack-work

