typecheckSources = Typecheck/TypeChecker.hs Typecheck/TypeCheckerData.hs Typecheck/TypeCheckerHelper.hs
interpretSources = Interpret/Interpreter.hs Interpret/InterpreterData.hs Interpret/InterpreterHelper.hs
syntaxSources = Syntax/AbsTempest.hs Syntax/ParTempest.hs Syntax/LexTempest.hs

all: interpreter

interpreter: Main.hs ${typecheckSources} ${interpretSources} ${syntaxSources}
	ghc --make Main.hs -o interpreter

clean:
	rm -f *.hi
	rm -f *.o
	rm -f interpreter
