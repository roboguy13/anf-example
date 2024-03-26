{-# LANGUAGE LambdaCase #-}

module NonMonadic
  where

import Data.Maybe (maybeToList)

import Syntax

toANF :: Expr -> Expr
toANF = fst . toANF' 0

toANF' :: Int -> Expr -> (Expr, Int)
toANF' unique = \case
  Var x -> (Var x, unique)
  Lit i -> (Lit i, unique)

  Apply f args ->
    let (letBinds, args', unique') = simplifyArgs unique args
    in
    (buildLets letBinds (Apply f args'), unique')

  Add a b ->
    let (a', unique') = toANF' unique a
        (b', unique'') = toANF' unique' b
    in
    (Add a' b', unique'')

  IfThenElse c t f ->
    let (c', unique') = toANF' unique c
        (t', unique'') = toANF' unique' t
        (f', unique''') = toANF' unique'' f
    in
    (IfThenElse c' t' f', unique''')

-- Bring "compound expressions" (non-variable, non-literals) into
-- let bindings. Leave non-compound expressions the unchanged.
simplifyArgs :: Int -> [Expr] -> ([LetBind], [Expr], Int)
simplifyArgs unique [] = ([], [], unique)
simplifyArgs unique (e : es) =
  let (maybeLetBind, e', unique') = simplifyArg unique e
      (letBinds, es', unique'') = simplifyArgs unique' es
  in
  (maybeToList maybeLetBind ++ letBinds, e' : es', unique'')

simplifyArg :: Int -> Expr -> (Maybe LetBind, Expr, Int)
simplifyArg unique (Var x) = (Nothing, Var x, unique)
simplifyArg unique (Lit i) = (Nothing, Lit i, unique)
simplifyArg unique compoundExpr =
  let (name, unique') = freshName unique
      letBind = LetBind name compoundExpr
  in
  (Just letBind, Var name, unique')

freshName :: Int -> (Name, Int)
freshName unique = (Name "internalName" unique, unique + 1)

toANFList :: Int -> [Expr] -> ([Expr], Int)
toANFList unique [] = ([], unique)
toANFList unique (x:xs) =
  let (x', unique') = toANF' unique x
      (xs', unique'') = toANFList unique' xs
  in
  (x' : xs', unique'')

