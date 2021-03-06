module Ski where
--import Test.QuickCheck
import Prelude hiding (abs)
import qualified List

data C = S | K | I | P | C :@: C | CVar String
  deriving (Eq)
           
bracket s = "(" ++ s ++ ")"
nobracket s = s

cs br (c1 :@: c2) = br (cs nobracket c1 ++ " :@: " ++ cs bracket c2)
cs br S = "S"
cs br K = "K"
cs br I = "I"
cs br P = "P"
cs br (CVar x) = x

instance Show C where
  show = cs nobracket
  

cnormal :: C -> C
cnormal (t :@: t') =
  case (cnormal t, cnormal t') of
    (I, x) -> x
    (K :@: x, y) -> x
    ((S :@: x) :@: y, z) -> cnormal $ (x :@: z) :@: (y :@: z)
    (P, _) -> I
    (f, a) -> f :@: a
cnormal base = base

isNormal c = cnormal c == c

cterms :: [[C]]
leaves :: [C]
appnodes :: Int -> [C]

increase :: [[C]] -> [C]
  -- argument: C terms with 0..n-1 app nodes
  -- result  : C terms with n app nodes
increase css = concat $ zipWith combine css (reverse css)
    where combine cs1 cs2 = filter isNormal [ c1 :@: c2 | c1 <- cs1, c2 <- cs2 ]
leaves = [S, K, I, P]
appnodes 0 = leaves
appnodes n = increase (take n cterms)
cterms = map appnodes [0..]

equiv c args c' = cnormal (foldl (\ c x -> c :@: CVar x) c args) == cnormal c'

isCompose c = equiv c ["f", "g", "x"] (CVar "f" :@: (CVar "g" :@: CVar "x"))
isApp c = equiv c ["f", "x"] (cf :@: cx)

findC :: (C -> Bool) -> C

findC p = search cterms
   where search (cs:css) = case List.find p cs of
                             Just c -> c
                             Nothing -> search css


compose' = S :@: (K :@: S) :@: K

cf = CVar "f"
cg = CVar "g"
cx = CVar "x"
cy = CVar "y"
cz = CVar "z"

-- N.B. it would be good to automate normalization and with 
-- extensional equality

-- note that for any x, S (KK) I x == K x


data Lam = Lam String Lam
         | App Lam Lam
         | Var String
  deriving (Show)
           
trans :: Lam -> C
abs   :: String -> C -> C

trans (Lam x e)
  | App f (Var y) <- e, y == x, not (x `freeIn` f) = trans f
  | otherwise = abs x (trans e)
trans (App f a) = trans f :@: trans a
trans (Var x)   = CVar x

abs x (CVar y) 
  | x == y = I
  | x /= y = K :@: CVar y
abs x (t :@: t') = (S :@: abs x t) :@: abs x t'
abs x c = K :@: c


{-
-- next is from wikipedia on combinatory logic
ctrans (Var x) = CVar x
ctrans (App e1 e2) = ctrans e1 :@: ctrans e2
ctrans (Lam x e)
  | App f (Var y) <- e, y == x, not (x `freeIn` f) = ctrans f
  | not (x `freeIn` e) = K :@: ctrans e
  | Var y <- e, y == x = I
  | Lam y e' <- e = twolambda (ctrans e)
  | App e1 e2 <- e = S :@: ctrans (Lam x e1) :@: ctrans (Lam x e2)
 where twolambda y (c1 :@: c2) = S :@:
-}



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

x, y, z, f, g :: Lam
x = Var "x"
y = Var "y"
z = Var "z"
f = Var "f"
g = Var "g"

nfp = normal_forms_preserved


compose = Lam "f" $ Lam "g" $ Lam "x" $ App f (App g x)

self = Lam "f" $ compose `App` (Lam "_" f) `App` f
