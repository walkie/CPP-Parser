
type Name = String

data Expr = Var Name 
          | Not Expr
          | And Expr Expr
          | Or  Expr Expr
          deriving (Eq, Show)

data Cond a = CN
            | CD  a
            | IT  Expr (Cond a)
            | ITE Expr (Cond a) (Cond a)
            deriving (Eq, Show)

data Simple a = SN
              | SD a
              | SC Name (Simple a) (Simple a)
              deriving (Eq, Show)

simplify :: Cond a -> Simple a

simplify CN       = SN
simplify (CD d)   = SD d
simplify (IT c t) = simplify (ITE c t CN)

simplify (ITE (Var v) t e) = SC v (simplify t) (simplify e)
simplify (ITE (Not c) t e) = simplify (ITE c e t)

simplify (ITE (And c d) t e) = simplify (ITE c (ITE d t e) e)
simplify (ITE (Or  c d) t e) = simplify (ITE c t (ITE d t e)) -- data duplication!

