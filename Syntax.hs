module Syntax
  where

data Name = Name String Int
  deriving (Show)

data LetBind = LetBind Name Expr
  deriving (Show)

data Expr
  = Var Name
  | Lit Int
  | Apply String [Expr] -- Function application
  | LetIn LetBind Expr
  | Add Expr Expr
  | IfThenElse Expr Expr Expr
  deriving (Show)

-- | Make a @Name@ out of a surface syntax name
mkName :: String -> Name
mkName x = Name x 0

-- [letBnd1, letBnd2] eBody
--   --->
-- LetIn letBnd1 (LetIn letBnd2 eBody)
buildLets :: [LetBind] -> Expr -> Expr
buildLets letBinds e = foldr LetIn e letBinds

