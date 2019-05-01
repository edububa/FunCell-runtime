-- |This module contains a library of functions to deal with the
-- @ExternalModule@
module Lib.ExternalModule where

-- external imports
import Control.Monad.Trans.Except
import Data.Cell
import Data.ExternalModule
import Language.Haskell.Interpreter as I
import Data.Either.Combinators (mapLeft)

-- |'saveAndLoadExternalModule' first saves the content of the
-- @ExternalModule@ using 'saveExternalModuleFile', then checks if
-- compiles loading it on a GHC interpreter session.
saveAndLoadExternalModule :: ExternalModule -> ExceptT Error IO ()
saveAndLoadExternalModule (ExternalModule input) = ExceptT $ do
  liftIO $ saveExternalModuleFile input
  res <- liftIO $ runInterpreter $ I.loadModules ["ExternalModule.hs"]
  return . mapLeft (const "Won't compile") $ res

-- |'saveExternalModuleFile' writes a file with the content of the
-- @ExternalModule@. This file is called @ExternalModule.hs@ and is
-- located in the path where the program is executed.
saveExternalModuleFile :: String -> IO ()
saveExternalModuleFile input = do
  writeFile "ExternalModule.hs" ("module ExternalModule where \n" <> input)
