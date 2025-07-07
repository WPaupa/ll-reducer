module CheckWellFormedness where

import AbsLL
import PrintLL

appearances :: Ident -> Term -> Integer
appearances x (TLambda y t) =
    if x == y then 0 else appearances x t
appearances x (TLamBang y t) =
    if x == y then 0 else appearances x t
appearances x (TVar y) =
    if x == y then 1 else 0
appearances x (TBang t) = appearances x t
appearances x (TApp t1 t2) =
    appearances x t1 + appearances x t2

hasnonzeros :: Ident -> Term -> Bool
hasnonzeros x (TLambda y t) =
    if x == y then False else hasnonzeros x t
hasnonzeros x (TLamBang y t) =
    if x == y then False else hasnonzeros x t
hasnonzeros x (TVar y) = False
hasnonzeros x (TBang t) = appearances x t > 0
hasnonzeros x (TApp t1 t2) = hasnonzeros x t1 || hasnonzeros x t2

hasnonones :: Ident -> Term -> Bool
hasnonones x (TLambda y t) =
    if x == y then False else hasnonones x t
hasnonones x (TLamBang y t) =
    if x == y then False else hasnonones x t
hasnonones x (TVar y) = x == y
hasnonones x (TBang t) = hasnonzeros x t
hasnonones x (TApp t1 t2) = hasnonones x t1 || hasnonones x t2

check :: Term -> IO Bool
check (TLambda x t) =
    if appearances x t > 1 then do
        putStrLn $ "The variable " ++ printTree x ++ " appears in the term '" ++ printTree t ++ "' " ++ show (appearances x t) ++ " times"
        return False
    else if hasnonzeros x t then do
        putStrLn $ "The variable " ++ printTree x ++ " appears in the term '" ++ printTree t ++ "' at a non-zero depth"
        return False
    else check t
check (TLamBang x t) =
    if hasnonones x t then do
        putStrLn $ "The variable " ++ printTree x ++ " appears in the term '" ++ printTree t ++ "' at a non-one depth"
        return False
    else check t
check (TVar x) = return True
check (TBang t) = check t
check (TApp t1 t2) = do
    c1 <- check t1
    if c1 then check t2 else return False