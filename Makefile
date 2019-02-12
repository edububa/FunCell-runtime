docs:
	stack exec -- haddock --html app/Main.hs src/ParsingLib.hs  src/Data/Cell/Lib.hs src/Data/Cell.hs src/Lib/Eval.hs src/Lib/Indexing.hs  --hyperlinked-source --odir=dist/docs

build:
	stack build

clean:
	rm -rf .stack-work
	rm -rf dist
	stack clean
