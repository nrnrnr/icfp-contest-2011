module Ski where
import Test.QuickCheck
import Prelude hiding (abs)

data C = S | K | I | C :@: C | CVar String
  deriving (Show, Eq)

cnormal :: C -> C
cnormal (t :@: t') =
  case (cnormal t, cnormal t') of
    (I, x) -> x
    (K :@: x, y) -> x
    ((S :@: x) :@: y, z) -> cnormal $ (x :@: z) :@: (y :@: z)
    (f, a) -> f :@: a
cnormal base = base

-- note that for any x, S (KK) I x == K x


data Lam = Lam String Lam
         | App Lam Lam
         | Var String
  deriving (Show)
           
trans :: Lam -> C
abs   :: String -> C -> C

trans (Lam x e) = abs x (trans e)
trans (App f a) = trans f :@: trans a
trans (Var x)   = CVar x

abs x (CVar y) 
  | x == y = I
  | x /= y = K :@: CVar y
abs x (t :@: t') = (S :@: abs x t) :@: abs x t'
abs x c = K :@: c



lnormal :: Lam -> Lam
lnormal (Lam x e) = Lam x (lnormal e)
lnormal (App f arg) =
  case (lnormal f, lnormal arg) of
    (Lam x body, arg) -> lnormal (subst arg x body)
    (f, arg) -> App f arg
lnormal (Var x) = Var x

subst :: Lam -> String -> Lam -> Lam
subst arg x = theta
  where theta (Var y)
          | y == x = arg
          | otherwise = Var y
        theta (App f a) = App (theta f) (theta a)
        theta (Lam y body)
          | not (y `freeIn` arg) || not (x `freeIn` body) = Lam y (theta body)
          | otherwise = theta (Lam w (subst (Var w) y body))
              where w = fresh y (App arg body)
                    
fresh y term = if y `freeIn` term then fresh (y++"'") term
               else y
                    
freeIn :: String -> Lam -> Bool
freeIn x (Var y) = x == y
freeIn x (App f a) = x `freeIn` f || x `freeIn` a
freeIn x (Lam y b) = x /= y && x `freeIn` b


normal_forms_preserved e = cnormal (trans e) == cnormal (trans (lnormal e))

x, y, z :: Lam
x = Var "x"
y = Var "y"
z = Var "z"

nfp = normal_forms_preserved
