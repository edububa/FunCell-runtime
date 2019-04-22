docs:
	stack exec -- haddock --html app/Main.hs src/Data/Cell.hs src/Data/Parsing.hs src/Data/Dependency.hs src/Data/ServerState.hs src/Data/ExternalModule.hs src/Lib/Application.hs src/Lib/ExternalModule.hs src/Lib/Cell.hs src/Lib/Indexing.hs src/Lib/CodeGen.hs src/Lib/Parsing.hs src/Lib/Dependency.hs src/Lib/ServerState.hs src/Lib/Eval.hs --hyperlinked-source --odir=dist/docs

build:
	stack build

clean:
	rm -rf .stack-work
	rm -rf dist
	stack clean
