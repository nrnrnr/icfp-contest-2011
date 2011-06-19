module Instr where -- instructions
import Test.QuickCheck
import Prelude hiding (abs)
import qualified List

data C = S | K | I | P | C :@: C | Get
       | Slot Int | Lit Int | CVar String | Zero | Succ | Dbl
  deriving (Eq, Ord)

data Instruction = ApplySlot C
                 | ApplyCard C

instance Show Instruction where
  show (ApplySlot c) = "a[i] := a[i] " ++ show c
  show (ApplyCard c) = "a[i] := " ++ show c ++ " a[i]"


update slot (ApplySlot c) = cnormal (slot :@: c)
update slot (ApplyCard c) = cnormal (c :@: slot)

run = foldl update (Slot 17)

as = ApplySlot
ac = ApplyCard

applyTo0 = [ac K, ac S, as Get, as Zero]
applyTo1 = [ac K, ac S, as Get, ac K, ac S, as Succ, as Zero]


data Numeral = Z | Apply C Numeral
  deriving (Show)

applyToSlotNumeraled n = [ac K, ac S, as Get] ++ toNum n
    where toNum Z = [as Zero]
          toNum (Apply c n) = [ac K, ac S, as c] ++ toNum n

applyToNumeral n = toNum n
    where toNum Z = [as Zero]
          toNum (Apply c n) = [ac K, ac S, as c] ++ toNum n

numeral :: Int -> Numeral
number  :: Numeral -> Int

numeral 0 = Z
numeral n = if odd n then Apply Succ hi else hi
    where hi = double (numeral (n `div` 2))
          double Z = Z
          double n = Apply Dbl n

number Z = 0
number (Apply Succ n) = number n + 1
number (Apply Dbl n) = 2 * number n


n_left_inverse n = n >= 0 ==> number (numeral n) == n

applyToN n = applyToSlotNumeraled (numeral n)

apply_law n = n >= 0 ==> run (applyToN n) == Slot 17 :@: Slot n

apply_num_law n = n >= 0 ==>
                  run (applyToNumeral (numeral n)) == Slot 17 :@: Lit n

apps :: C -> Int
apps (c :@: c') = apps c + apps c' + 1
apps _ = 0

ccomp c c' = case compare (apps c) (apps c') of
               EQ  -> compare c c'
               ord -> ord

csort = List.sortBy ccomp           

{-
nslots (Slot _) = 1
nslots (c :@: c') = nslots c + nslots c'
nslots _ = 0
-}

           
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
cs br Succ = "succ"
cs br Dbl = "dbl"

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
    (Get, _) -> error "get from non-integer"
    (Succ, Lit n) -> Lit (n+1)
    (Succ, _) -> error "succ from non-integer"
    (Dbl, Lit n) -> Lit (n+n)
    (Dbl, _) -> error "dbl from non-integer"
    (f, a) -> f :@: a
cnormal Zero = Lit 0
cnormal base = base

isNormal c = cnormal c == c

apply_slot_to_0 = S :@: (K :@: Slot 17) :@: Get :@: Zero


equiv c args c' = cnormal (foldl (\ c x -> c :@: CVar x) c args) == cnormal c'

isCompose c = equiv c ["f", "g", "x"] (CVar "f" :@: (CVar "g" :@: CVar "x"))
isApp c = equiv c ["f", "x"] (cf :@: cx)


compose' = S :@: (K :@: S) :@: K

cf = CVar "f"
cg = CVar "g"
cx = CVar "x"
cy = CVar "y"
cz = CVar "z"

-- N.B. it would be good to automate normalization and with 
-- extensional equality

-- note that for any x, S (KK) I x == K x


