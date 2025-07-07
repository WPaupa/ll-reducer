module Main where

import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import Control.Monad      ( when )
import System.IO
import Data.Map ( insert, empty )

import AbsLL
import Env
import LexLL   ( Token, mkPosToken )
import ParLL   ( pDef, myLexer )
import PrintLL ( Print, printTree )
import SkelLL  ()
import Reduce

type Err        = Either String
type ParseFun a = [Token] -> Err a
type Verbosity  = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

run :: Env -> Verbosity -> ParseFun Def -> String -> IO Env
run env v p s =
  case p ts of
    Left err -> do
      putStrLn "\nParse              Failed...\n"
      putStrV v "Tokens:"
      mapM_ (putStrV v . showPosToken . mkPosToken) ts
      putStrLn err
      return env
    Right (Def name tree) -> do
      let tree' = insertEnv tree env
      putStrLn $ "Replaced declarations: " ++ printTree tree 
      hSetBuffering stdin NoBuffering
      tree'' <- reduceKeypress tree'
      hSetBuffering stdin LineBuffering
      return $ insert name tree'' env
  where
  ts = myLexer s
  showPosToken ((l,c),t) = concat [ show l, ":", show c, "\t", show t ]

addDefinition :: Env -> IO Env
addDefinition env = do
    text <- getLine
    if text == "" then addDefinition env else do
        env' <- run env 2 pDef text 
        addDefinition env'

main :: IO ()
main = do
    addDefinition empty
    return ()
