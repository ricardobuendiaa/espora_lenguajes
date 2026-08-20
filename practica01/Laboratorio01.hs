module Laboratorio01 where
import Data.Bits (Bits(xor))

distanciaOrigen :: Double -> Double -> Double

distanciaOrigen x y = sqrt ((0 - x)^2 + (0 - y)^2)

sumaCuadradosPares :: [Int] -> Int

sumaCuadradosPares xs = sum (map (^2)(filter even xs))

aplicaTresVeces :: (a -> a) -> a -> a
aplicaTresVeces f x = f (f (f x))

varianza2 :: Double -> Double -> Double
varianza2 x y = ((x-mu)^2 + (y-mu)^2) / 2
  where mu =  (x + y)/2

clasificaTemperatura :: Int -> String
clasificaTemperatura c 
  | c < 1 = "frio extremo"
  | c <= 15 = "frio"
  | c < 26 = "templado"
  | c < 36 = "calido"
  | otherwise = "calor extremo"

intercala :: a -> [a] -> [a]
intercala _ [] = []
intercala _ [x] = [x]
intercala sep (x:xs) = x : sep : intercala sep xs

data Expr
  = Lit Int
  | Suma Expr Expr
  | Producto Expr Expr
  deriving (Eq, Show)

evalua :: Expr -> Int
evalua (Lit n) = n
evalua (Suma e1 e2) = evalua e1 + evalua e2
evalua (Producto e1 e2) = evalua e1 * evalua e2