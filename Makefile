docs:
	stack exec -- haddock --html app/Main.hs src/Data/Cell.hs src/Data/ExternalModule.hs src/Data/ServerState.hs src/Lib/Application.hs src/Lib/CodeGen.hs src/Lib/Eval.hs src/Lib/Indexing.hs src/Lib/Cell.hs src/Lib/Dependency.hs src/Lib/ExternalModule.hs src/Lib/ServerState.hs --hyperlinked-source --odir=dist/docs

build:
	stack build

clean:
	rm -rf .stack-work
	rm -rf dist
	stack clean
