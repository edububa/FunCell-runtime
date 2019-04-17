{-| This module contains a library of functions to deal with the
  @ExternalModule@ -}
module Lib.ExternalModule where

-- external imports
import Control.Monad.Trans.Except
import Data.Cell
import Data.ExternalModule
import Language.Haskell.Interpreter as I
import Data.Either.Combinators (mapLeft)

saveAndLoadExternalModule :: ExternalModule -> ExceptT Error IO ()
saveAndLoadExternalModule (ExternalModule input) = ExceptT $ do
  liftIO $ saveExternalModuleFile input
  res <- liftIO $ runInterpreter $ I.loadModules ["ExternalModule.hs"]
  return . mapLeft (const "Won't compile") $ res

saveExternalModuleFile :: String -> IO ()
saveExternalModuleFile input = do
  writeFile "ExternalModule.hs" ("module ExternalModule where \n" <> input)
