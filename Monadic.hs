{-# LANGUAGE LambdaCase #-}

module Monadic
  where

import Data.Maybe (maybeToList, catMaybes)
import Control.Monad.State
import Control.Applicative

import Syntax

newtype Fresh a = Fresh { runFresh :: State Int a }
  deriving (Functor, Applicative, Monad)

toANF :: Expr -> Expr
toANF e = evalState (runFresh (toANF' e)) 0

toANF' :: Expr -> Fresh Expr
toANF' = \case
  Var x -> pure (Var x)
  Lit i -> pure (Lit i)

  Apply f args -> do
    (letBinds, args') <- simplifyArgs args
    pure (buildLets letBinds (Apply f args'))
  
  Add a b ->
    liftA2 Add (toANF' a) (toANF' b)

  IfThenElse c t f ->
    liftA3 IfThenElse (toANF' c) (toANF' t) (toANF' f)

-- Bring "compound expressions" (non-variable, non-literals) into
-- let bindings. Leave non-compound expressions the unchanged.
simplifyArgs :: [Expr] -> Fresh ([LetBind], [Expr])
simplifyArgs es = do
  (letBindMaybes, es') <- fmap unzip (traverse simplifyArg es)
  pure (catMaybes letBindMaybes, es')

simplifyArg :: Expr -> Fresh (Maybe LetBind, Expr)
simplifyArg (Var x) = pure (Nothing, Var x)
simplifyArg (Lit i) = pure (Nothing, Lit i)
simplifyArg compoundExpr = do
  name <- freshName
  let letBind = LetBind name compoundExpr
  pure (Just letBind, Var name)

freshName :: Fresh Name
freshName = do
  unique <- Fresh get
  Fresh (put (unique + 1))
  pure (Name "internalName" unique)

