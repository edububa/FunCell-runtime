module EvalLib where

import Language.Haskell.Interpreter as I
import Data.Either.Combinators (mapLeft)

parseAndEval "" = return $ Right ""
parseAndEval x  = do
  res <- I.runInterpreter $ do {setImports ["Prelude"]; eval x}
  return $ mapLeft show res
