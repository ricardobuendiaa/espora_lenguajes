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

sust (Let bs body) x s
  | x `elem` xs = Let bs' body
  | otherwise   = Let bsRen (sust bodyRen x s)
  where
    xs = map fst bs
    bs' = map (\(y, e) -> (y, sust e x s)) bs
    (bsRen, bodyRen) = renombra (freeVars s) bs' body (names (Let bs body) ++ names s ++ [x])

sust (LetStar [] body) x s = LetStar [] (sust body x s)
sust (LetStar ((y, e):bs) body) x s
  | y == x    = LetStar ((y, e'):bs) body
  | y `elem` freeVars s =
      let z = freshName (names resto ++ names s ++ [x, y])
          LetStar bsRen bodyRen = sust (sust resto y (Id z)) x s
       in LetStar ((z, e'):bsRen) bodyRen
  | otherwise =
      let LetStar bs' body' = sust resto x s
       in LetStar ((y, e'):bs') body'
  where
    e' = sust e x s
    resto = LetStar bs body

renombra :: [String] -> [Binding] -> ASA -> [String] -> ([Binding], ASA)
renombra _ [] body _ = ([], body)
renombra libres ((y, e):bs) body usados
  | y `notElem` libres =
      let (bsRen, bodyRen) = renombra libres bs body usados
       in ((y, e) : bsRen, bodyRen)
  | otherwise =
      let z = freshName usados
          body' = sust body y (Id z)
          (bsRen, bodyRen) = renombra libres bs body' (z : usados)
       in ((z, e) : bsRen, bodyRen)

sustMany :: ASA -> [Binding] -> ASA
sustMany e bs = foldl remplaza e' (zip bs temps)
    where
        temps = ["_t" ++ show n | n <- [0..]]
        e' = foldl cambia e (zip bs temps)
        cambia acc ((x, _),t) = sust acc x (Id t)
        remplaza acc ((_, e), t) = sust acc t e 

-- RETO 4: semantica operacional de paso grande
-- let es simultaneo; let* se evalua directamente, asociacion por asociacion.
bigStep :: ASA -> Maybe ASA
bigStep (Num n) = Just (Num n)
bigStep (Boolean b) = Just (Boolean b)
bigStep (Id _) = Nothing
bigStep (And es) = do
  vs <- mapM bigStep es
  bs <- mapM valorBool vs
  Just (Boolean (and bs))
bigStep (Or es) = do
  vs <- mapM bigStep es
  bs <- mapM valorBool vs
  Just (Boolean (or bs))
bigStep (Add es) = Num . sum <$> numeros es
bigStep (Mul es) = Num . product <$> numeros es
bigStep (Sub es) = Num . restaTruncada <$> numeros es
bigStep (Div es) = do
  ns <- numeros es
  if all (/= 0) (tail ns) then Just (Num (division ns)) else Nothing
bigStep (Lt es) = comparacion (<) es
bigStep (Gt es) = comparacion (>) es
bigStep (Le es) = comparacion (<=) es
bigStep (Ge es) = comparacion (>=) es
bigStep (Expt e1 e2) = do
  Num n <- bigStep e1
  Num m <- bigStep e2
  Just (Num (n ^ m))
bigStep (EqP e1 e2) = do
  v1 <- bigStep e1
  v2 <- bigStep e2
  case (v1, v2) of
    (Num n, Num m) -> Just (Boolean (n == m))
    (Boolean b, Boolean c) -> Just (Boolean (b == c))
    _ -> Nothing
bigStep (Not e) = do
  v <- bigStep e
  case v of
    Boolean b -> Just (Boolean (not b))
    Num _ -> Just (Boolean False)
    _ -> Nothing
bigStep (Add1 e) = unaryNum (+ 1) e
bigStep (Sub1 e) = unaryNum (max 0 . subtract 1) e
bigStep (ZeroP e) = do
  Num n <- bigStep e
  Just (Boolean (n == 0))
bigStep (Let bs body)
  | length xs /= length (nub xs) = Nothing
  | otherwise = do
      vs <- mapM (bigStep . snd) bs
      bigStep (sustMany body (zip xs vs))
  where xs = map fst bs
bigStep (LetStar [] body) = bigStep body
bigStep (LetStar ((x, e):bs) body) = do
  v <- bigStep e
  bigStep (sust (LetStar bs body) x v)

valorBool :: ASA -> Maybe Bool
valorBool (Boolean b) = Just b
valorBool _ = Nothing

valorNum :: ASA -> Maybe Int
valorNum (Num n) = Just n
valorNum _ = Nothing

numeros :: [ASA] -> Maybe [Int]
numeros es = mapM (\e -> bigStep e >>= valorNum) es

restaTruncada :: [Int] -> Int
restaTruncada (n:ns) = foldl (\acc m -> max 0 (acc - m)) n ns
restaTruncada [] = 0

division :: [Int] -> Int
division (n:ns) = foldl div n ns
division [] = 0

comparacion :: (Int -> Int -> Bool) -> [ASA] -> Maybe ASA
comparacion op es = do
  ns <- numeros es
  Just (Boolean (and (zipWith op ns (tail ns))))

unaryNum :: (Int -> Int) -> ASA -> Maybe ASA
unaryNum op e = do
  Num n <- bigStep e
  Just (Num (op n))
