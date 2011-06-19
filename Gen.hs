module Gen where
--import Test.QuickCheck
import Prelude hiding (abs)
import qualified List

data C = S | K | I | P | C :@: C | Get | Slot Int | Lit Int | CVar String | Zero
  deriving (Eq, Ord)

apps :: C -> Int
apps (c :@: c') = apps c + apps c' + 1
apps _ = 0

ccomp c c' = case compare (apps c) (apps c') of
               EQ  -> compare c c'
               ord -> ord

csort = List.sortBy ccomp           

nslots (Slot _) = 1
nslots (c :@: c') = nslots c + nslots c'
nslots _ = 0


           
bracket s = "(" ++ s ++ ")"
nobracket s = s

cs br (c1 :@: c2) = br (cs nobracket c1 ++ " :@: " ++ cs bracket c2)
cs br S = "S"
cs br K = "K"
cs br I = "I"
cs br P = "put"
cs br (Slot i) = "a[" ++ show i ++ "]"
cs br Get = "get"
cs br (Lit n) = show n
cs br (CVar x) = x
cs br Zero = "zero"

instance Show C where
  show = cs nobracket
  

cnormal :: C -> C
cnormal (t :@: t') =
  case (cnormal t, cnormal t') of
    (I, x) -> x
    (K :@: x, y) -> x
    ((S :@: x) :@: y, z) -> cnormal $ (x :@: z) :@: (y :@: z)
    (P, x) -> I
    (Get, Lit n) -> Slot n
    (f, a) -> f :@: a
cnormal Zero = Lit 0
cnormal base = base

isNormal c = cnormal c == c

leaves :: [C]
appnodes :: Int -> [C]

increase :: [[C]] -> [C]
  -- argument: C terms with 0..n-1 app nodes
  -- result  : C terms with n app nodes
increase css = concat $ zipWith combine css (reverse css)
    where combine cs1 cs2 = filter isNormal [ c1 :@: c2 | c1 <- cs1, c2 <- cs2 ]
cards = [S, K, I, P, Get, Zero]
slots = [Slot 99, Slot 88, Lit 99, Lit 88]
leaves = cards ++ slots
appnodes 0 = leaves
appnodes n = increase (take n cterms')
cterms' = map appnodes [0..]
cterms :: [[Result]] -- terms we can get into a slot
cterms = map result (slots ++ cards) : map next cterms

data Result = Result { source :: C, normal :: C }
instance Eq Result where
  r == r' = normal r `eqApp` normal r'
    where eqApp (e1 :@: e2) (e1' :@: e2') = eqApp e1 e1' && eqApp e2 e2'
          eqApp (Slot x) (Slot y) = x == y || (x /= 0 && y /= 0)
          eqApp (Lit  x) (Lit  y) = x == y || (x /= 0 && y /= 0)
          eqApp e e' = e == e'

instance Show Result where
  show r = show (source r) ++ " ===> " ++ show (normal r)

result :: C -> Result
result c = Result c (cnormal c)

appr (Result s n) (Result s' n') = Result (s :@: s') (cnormal (n :@: n'))

next :: [Result] -> [Result] -- add an app node to a card or to a slot
next terms =
  List.nub $ [card `appr` term | card <- map result cards, funOk card, term <- terms] ++
             [term `appr` card | card <- map result cards, term <- terms, funOk term]

funOk r = ok (normal r)
    where ok (Lit _) = False
          ok (Zero) = False
          ok _ = True


apply88_0 = S :@: (K :@: Slot 88) :@: Get :@: Zero


equiv c args c' = cnormal (foldl (\ c x -> c :@: CVar x) c args) == cnormal c'

isCompose c = equiv c ["f", "g", "x"] (CVar "f" :@: (CVar "g" :@: CVar "x"))
isApp c = equiv c ["f", "x"] (cf :@: cx)

isSlotApp0 c = equiv c [] (Slot 88 :@: Slot 0)

findC :: (C -> Bool) -> Result

findC p = search cterms
   where search (cs:css) = case List.find (p . normal) cs of
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


