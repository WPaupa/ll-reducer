module Reduce where

import AbsLL
import Env
import Data.Map
import Control.Monad.Trans.Reader
import PrintLL ( Print, printTree )


replace :: Term -> Ident -> Term -> Term
replace m x (TVar y) = 
    if x == y then m else TVar y
replace m x (TLambda y n) =
    if x == y then TLambda y n else TLambda y (replace m x n)
replace m x (TLamBang y n) =
    if x == y then TLamBang y n else TLamBang y (replace m x n)
replace m x (TBang n) = TBang (replace m x n)
replace m x (TApp n1 n2) = TApp (replace m x n1) (replace m x n2)

stepReduce :: Term -> IO Term
stepReduce (red@(TApp (TLambda x n) m)) = do
    putStrLn $ "Found beta-redex: " ++ printTree red
    let result = replace m x n
    putStrLn $ "Reduced to: " ++ printTree result
    return result

stepReduce (red@(TApp (TLamBang x n) (TBang m))) = do
    putStrLn $ "Found bang-redex: " ++ printTree red
    let result = replace m x n
    putStrLn $ "Reduced to: " ++ printTree result
    return result

stepReduce (TLambda x n) = fmap (TLambda x) (stepReduce n)
stepReduce (TLamBang x n) = fmap (TLamBang x) (stepReduce n)
stepReduce (TVar x) = return (TVar x)
stepReduce (TBang n) = fmap TBang (stepReduce n)
stepReduce (TApp n m) = do
    n' <- stepReduce n
    if n' == n then do
        m' <- stepReduce m
        return $ TApp n' m'
    else
        return $ TApp n' m

reduceKeypress :: Term -> IO Term 
reduceKeypress t = do
    t' <- stepReduce t
    putStrLn $ "Current term: " ++ printTree t'
    if t' == t then do
        putStrLn "No further reductions to make, waiting for more declarations..."
        return t
    else do
        getChar
        reduceKeypress t'

insertEnv :: Term -> Env -> Term
insertEnv = foldrWithKey (\x m n -> replace m x n)