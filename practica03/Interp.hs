module Interp where

import Grammars
import Data.List (nub)

-- RETO 3: sustitucion nominal que evita captura
freeVars :: ASA -> [String]

freeVars (Id x) = [x]
freeVars (Num _) = []
freeVars (Boolean _) = []

freeVars (And es) = concatMap freeVars es
freeVars (Or es) = concatMap freeVars es
freeVars (Add es) = concatMap freeVars es
freeVars (Sub es) = concatMap freeVars es
freeVars (Mul es) = concatMap freeVars es
freeVars (Div es) = concatMap freeVars es
freeVars (Lt es) = concatMap freeVars es
freeVars (Gt es) = concatMap freeVars es
freeVars (Le es) = concatMap freeVars es
freeVars (Ge es) = concatMap freeVars es

freeVars (Expt e1 e2) = freeVars e1 ++ freeVars e2
freeVars (EqP e1 e2) = freeVars e1 ++ freeVars e2

freeVars (Not e) = freeVars e
freeVars (Add1 e) = freeVars e
freeVars (Sub1 e) = freeVars e
freeVars (ZeroP e) = freeVars e

freeVars (Let bs body) = concatMap (freeVars . snd) bs ++ filter (`notElem` map fst bs) (freeVars body)

freeVars (LetStar [] body) = freeVars body

freeVars (LetStar ((x, e):bs) body) = freeVars e ++ filter (/= x) (freeVars (LetStar bs body))


names :: ASA -> [String]
names (Id x) = [x]
names (Num _) = []
names (Boolean _) = []

names (And es) = concatMap names es
names (Or es) = concatMap names es
names (Add es) = concatMap names es
names (Sub es) = concatMap names es
names (Mul es) = concatMap names es
names (Div es) = concatMap names es
names (Lt es) = concatMap names es
names (Gt es) = concatMap names es
names (Le es) = concatMap names es
names (Ge es) = concatMap names es
names (Expt e1 e2) = names e1 ++ names e2
names (EqP e1 e2) = names e1 ++ names e2
names (Not e) = names e
names (Add1 e) = names e
names (Sub1 e) = names e
names (ZeroP e) = names e

names (Let bs body) = map fst bs ++ concatMap (names . snd) bs ++ names body
names (LetStar bs body) = map fst bs ++ concatMap (names . snd) bs ++ names body


freshName :: [String] -> String
freshName names = head (filter (`notElem` names) candidatos)
  where
    candidatos =  "z" : [ "z" ++ show n | n <- [0..] ]


sust :: ASA -> String -> ASA -> ASA
sust (Id y) x s
  | y == x    = s
  | otherwise = Id y

sust (Num n) _ _ = Num n
sust (Boolean b) _ _ = Boolean b

sust (And es) x s = And (map (\e -> sust e x s) es)
sust (Or es) x s = Or (map (\e -> sust e x s) es)
sust (Add es) x s = Add (map (\e -> sust e x s) es)
sust (Sub es) x s = Sub (map (\e -> sust e x s) es)
sust (Mul es) x s = Mul (map (\e -> sust e x s) es)
sust (Div es) x s = Div (map (\e -> sust e x s) es)
sust (Lt es) x s = Lt (map (\e -> sust e x s) es)
sust (Gt es) x s = Gt (map (\e -> sust e x s) es)
sust (Le es) x s = Le (map (\e -> sust e x s) es)
sust (Ge es) x s = Ge (map (\e -> sust e x s) es)

sust (Expt e1 e2) x s = Expt (sust e1 x s) (sust e2 x s)
sust (EqP e1 e2) x s = EqP (sust e1 x s) (sust e2 x s)
sust (Not e) x s = Not (sust e x s)
sust (Add1 e) x s = Add1 (sust e x s)
sust (Sub1 e) x s = Sub1 (sust e x s)
sust (ZeroP e) x s = ZeroP (sust e x s)


sustMany :: ASA -> [Binding] -> ASA
sustMany = undefined

-- RETO 4: semantica operacional de paso grande
-- let es simultaneo; let* se evalua directamente, asociacion por asociacion.
bigStep :: ASA -> Maybe ASA
bigStep = undefined