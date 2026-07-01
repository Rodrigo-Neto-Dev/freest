{- |
Module      :  Compiler.FreeST
Copyright   :  © The FreeST Team
Maintainer  :  freest-lang@listas.ciencias.ulisboa.pt

The entry point of the FreeST compiler.
-}
module Compiler.FreeST ( freest, runFreeST ) where

import Interpreter.Eval (evalModule, handleApplication)
import Interpreter.Value (emptyValueCtx, Value(..))
import UI.CLI ( RunOpts(..), opts, version, noModuleLoaded )
import Compiler.REPL ( ReplState(..), emptyReplState, repl )
import Compiler.Pipeline ( loadSilent )
import Interpreter.Exception ( printException )

import Control.Exception ( catch )
import Control.Monad ( void )
import Data.List ( find )
import Data.Map qualified as Map
import Syntax.Base ( external )
import Options.Applicative ( execParser )
import Prelude hiding ( lookup )
import System.Exit ( exitSuccess, exitFailure )

import System.IO (hFlush, stdout, stderr)

-- | The entry point of the FreeST compiler. Parses the command line options
-- and runs the compiler pipeline or else calls the REPL.
freest :: IO ()
freest = execParser opts >>= runFreeST

-- | Dispatch on the parsed command line options.
runFreeST :: RunOpts -> IO ()
runFreeST RunOpts{interactive = True, filePath = mPath, implicitPrelude = ip} =
  repl emptyReplState{filePath = mPath, implicitPrelude = ip}
runFreeST RunOpts{filePath = Nothing} =
  putStrLn (version ++ "\n" ++ noModuleLoaded) >>
  exitSuccess
runFreeST RunOpts{filePath = Just programPath, implicitPrelude = ip} = do
  putStrLn "DEBUG: runFreeST entered" >> hFlush stdout
  loadSilent ip programPath >>= \case
    Nothing -> putStrLn "DEBUG: loadSilent failed" >> hFlush stdout >> exitFailure
    Just (src, _, _, _, _, modl) -> do
      putStrLn "DEBUG: loadSilent succeeded" >> hFlush stdout
      catch
        (do
          putStrLn "DEBUG: about to evalModule" >> hFlush stdout
          vctx <- evalModule emptyValueCtx modl
          putStrLn "DEBUG: evalModule done" >> hFlush stdout
          let mMain = find (\(var, _) -> external var == "main") (Map.toList vctx)
          case mMain of
            Just (_, mainVal) -> do
              putStrLn "DEBUG: calling main" >> hFlush stdout
              res <- handleApplication emptyValueCtx mainVal []
              putStrLn ("DEBUG: main returned, res type=" ++ case res of VIO _ -> "VIO"; _ -> "other") >> hFlush stdout
              case res of
                VIO io -> void io
                _      -> return ()
            Nothing -> putStrLn "DEBUG: main not found" >> hFlush stdout
          exitSuccess)
        (\e -> printException src e >> exitFailure)