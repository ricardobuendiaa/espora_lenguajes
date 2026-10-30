module Laboratorio06 where

-- 1. Potencia --------------------------------------------------------------

-- Precondicion: el exponente es mayor o igual que cero.
potencia :: Int -> Int -> Int
potencia _ 0 = 1
potencia base exponente =
  base * potencia base (exponente - 1)

potenciaCola :: Int -> Int -> Int
potenciaCola n m = potenciaCola' n m 1  
  where
    potenciaCola' :: Int -> Int -> Int -> Int 
    potenciaCola' _ 0 acc = acc 
    potenciaCola' n m acc = potenciaCola' n (m - 1) (acc * n)

potenciaCPS :: Int -> Int -> (Int -> r) -> r
potenciaCPS n 0 k = k 1
potenciaCPS n m k = potenciaCPS n (m - 1) (\x -> k (n * x))

-- 2. Longitud de una lista -------------------------------------------------

longitud :: [a] -> Int
longitud [] = 0
longitud (_ : xs) = 1 + longitud xs

longitudCola :: [a] -> Int
longitudCola xs = auxiliar xs 0
    where
        auxiliar [] acc = acc
        auxiliar (_ : xs) acc = auxiliar xs (acc + 1)

longitudCPS :: [a] -> (Int -> r) -> r
longitudCPS xs k = auxiliarCPS xs 0 k
    where
        auxiliarCPS [] acc k = k acc
        auxiliarCPS (_ : xs) acc k = auxiliarCPS xs (acc + 1) k

-- 3. Reversa de una lista --------------------------------------------------

reversa :: [a] -> [a]
reversa [] = []
reversa (x : xs) = reversa xs ++ [x]

reversaCola :: [a] -> [a]
reversaCola xs = auxiliar xs []
  where
    auxiliar [] acc = acc
    auxiliar (x : xs) acc = auxiliar xs (x : acc)

reversaCPS :: [a] -> ([a] -> r) -> r
reversaCPS [] k = k []
reversaCPS (x : xs) k = reversaCPS xs (\v -> k (v ++ [x]))

-- 4. Concatenacion de una lista de listas ---------------------------------

concatena :: [[a]] -> [a]
concatena [] = []
concatena (xs : xss) = xs ++ concatena xss

concatenaCola :: [[a]] -> [a]
concatenaCola xss = auxiliar xss []
  where
    auxiliar [] acc = acc
    auxiliar (xs : xss) acc = auxiliar xss (acc ++ xs)

concatenaCPS :: [[a]] -> ([a] -> r) -> r
concatenaCPS [] k = k []
concatenaCPS (xs : xss) k = concatenaCPS xss (\v -> k (xs ++ v))

-- 5. Intercalado de dos listas --------------------------------------------

intercala :: [a] -> [a] -> [a]
intercala [] ys = ys
intercala xs [] = xs
intercala (x : xs) (y : ys) =
  x : y : intercala xs ys

intercalaCola :: [a] -> [a] -> [a]
intercalaCola xs ys = auxiliar xs ys []
  where
    auxiliar [] ys acc = acc ++ ys
    auxiliar xs [] acc = acc ++ xs
    auxiliar (x : xs) (y : ys) acc = auxiliar xs ys (acc ++ [x, y])

intercalaCPS :: [a] -> [a] -> ([a] -> r) -> r
intercalaCPS [] ys k = k ys
intercalaCPS xs [] k = k xs
intercalaCPS (x : xs) (y : ys) k = intercalaCPS xs ys (\v -> k (x : y : v))
