module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop =
    Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend st n v = \x -> if x == n then v else st x

empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE st (Var a) = st a
evalE _ (Val a) = a
evalE st (Op exp1 op exp2)
  | op == Plus = sum $ map (evalE st) $ [exp1, exp2]
  | op == Minus = (evalE st exp1) - (evalE st exp2)
  | op == Times = product $ map (evalE st) $ [exp1, exp2]
  | op == Divide = (evalE st exp1) `div` (evalE st exp2)
  | op == Gt = if(gt) then 1 else 0
  | op == Lt = if(lt) then 1 else 0
  | op == Ge = if(ge) then 1 else 0
  | op == Le = if(le) then 1 else 0
  | op == Eql = if(equ) then 1 else 0
     where equ = uncurry(==) xs
           gt = uncurry(>) xs
           lt = uncurry(<) xs
           ge = uncurry(<=) xs
           le = uncurry(<) xs
           xs = ((head $ vs), (last $ vs))
           vs = map (evalE st) $ [exp1, exp2]

--op :: ((a,a) -> a) -> (a, a) -> Boolean
--op :: _ b = uncurry f b

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Incr a) = DAssign a (Op (Var a) Plus (Val 1))
desugar (Assign n ex) = DAssign n ex
desugar (If ex th el) = DIf ex (desugar th) (desugar el)
desugar (While ex st) = DWhile ex (desugar(st))
desugar (For st ex st2 st3) = DSequence (desugar st) (DWhile ex (desugar(Sequence st2 st3)))
desugar (Sequence st1 st2) = DSequence (desugar st1) (desugar st2)
desugar (Skip) = DSkip
--Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple st (DAssign n ex) = extend st n $ evalE st ex
evalSimple st (DIf ex th el) = if(evalE st ex) /= 0 then evalSimple st th  else evalSimple st el
evalSimple st while@(DWhile ex dst) = if(evalE st ex) /= 0 then evalSimple (evalSimple st dst) while else st
evalSimple st (DSequence dst1 dst2) = evalSimple(evalSimple st dst1) dst2
evalSimple st (DSkip) = st

run :: State -> Statement -> State
run st statement = evalSimple st (desugar statement)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
