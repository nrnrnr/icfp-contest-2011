module Ski where
import Test.QuickCheck
import Prelude hiding (abs)

data C = S | K | I | C :@: C | CVar String
  deriving (Show)

cnormal :: C -> C
cnormal (t :@: t') =
  case (cnormal t, cnormal t') of
    (I, x) -> x
    (K :@: x, y) -> x
    ((S :@: x) :@: y, z) -> cnormal $ (x :@: z) :@: (y :@: z)
    (f, a) -> f :@: a
cnormal base = base


data Lam = Lam String Lam
         | App Lam Lam
         | Var String
           
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


