module Interp where

import Grammars

data ASA
  = Id Nombre
  | Num Int
  | Boolean Bool
  | Add ASA ASA
  | Sub ASA ASA
  | Not ASA
  | Fun Nombre ASA
  | App ASA ASA
  | If ASA ASA ASA
  deriving (Eq, Show)

data Value
  = NumV Int
  | BooleanV Bool
  | ClosureV Nombre ASA Env
  | ExprV ASA Env
  deriving (Eq, Show)

type Env = [(Nombre, Value)]

-- RETO 3: desazucarado ----------------------------------------------------

-- Recupera estas funciones del laboratorio 4. Las funciones y aplicaciones
-- del nucleo siguen siendo unarias, y las operaciones siguen siendo binarias.
curryFun :: [Nombre] -> ASA -> Maybe ASA
curryFun [] _ = Nothing
curryFun parametros cuerpo
  | repetidos parametros = Nothing
  | otherwise = Just (currifica parametros cuerpo)
  where
    currifica [] c = c
    currifica (x:xs) c = Fun x (currifica xs c)

    repetidos [] = False
    repetidos (x:xs) = x `elem` xs || repetidos xs

curryApp :: ASA -> [ASA] -> Maybe ASA
curryApp _ [] = Nothing
curryApp funcion argumentos = Just (aplica funcion argumentos)
  where
    aplica f [] = f
    aplica f (a:as) = aplica (App f a) as

binaryOp :: (ASA -> ASA -> ASA) -> [ASA] -> Maybe ASA
binaryOp _ [] = Nothing
binaryOp _ [_] = Nothing
binaryOp op (x:y:xs) = Just (combina (op x y) xs)
  where
    combina acumulado [] = acumulado
    combina acumulado (e:es) = combina (op acumulado e) es

-- Desazucara las clausulas ordinarias de cond en If anidados. La alternativa
-- else es el ultimo argumento y se conserva como la rama final.
desugarCond :: [(SASA, SASA)] -> SASA -> Maybe ASA
desugarCond [] alternativa = desugar alternativa
desugarCond ((condicion, rama):resto) alternativa =
  desugar (IfS condicion rama (CondS resto alternativa))

-- Elimina toda la sintaxis superficial. CondS se traduce a If anidados.
-- LetRecS f definicion cuerpo se traduce usando el identificador Y:
--
--   LetS f (AppS (IdS "Y") (FunS [f] definicion)) cuerpo
--
-- y despues se elimina tambien ese LetS. LetRecS no pertenece al nucleo.
desugar :: SASA -> Maybe ASA
desugar (IdS i) = Just (Id i)
desugar (NumS n) = Just (Num n)
desugar (BooleanS b) = Just (Boolean b)
desugar (NotS e) = niega (desugar e)
  where
    niega (Just e') = Just (Not e')
    niega Nothing = Nothing
desugar (AddS operandos) = suma (desugarLista operandos)
  where
    suma (Just es) = binaryOp Add es
    suma Nothing = Nothing
desugar (SubS operandos) = resta (desugarLista operandos)
  where
    resta (Just es) = binaryOp Sub es
    resta Nothing = Nothing
desugar (FunS parametros cuerpo) = funcion (desugar cuerpo)
  where
    funcion (Just cuerpo') = curryFun parametros cuerpo'
    funcion Nothing = Nothing
desugar (AppS funcion argumentos) =
  aplicacion (desugar funcion) (desugarLista argumentos)
  where
    aplicacion (Just f) (Just args) = curryApp f args
    aplicacion _ _ = Nothing
desugar (LetS nombre valor cuerpo) =
  desugar (AppS (FunS [nombre] cuerpo) [valor])
desugar (LetStarS ligaduras cuerpo) = desugar (anidaLets ligaduras cuerpo)
desugar (IfS condicion consecuente alternativa) =
  condicional (desugar condicion) (desugar consecuente) (desugar alternativa)
  where
    condicional (Just c) (Just t) (Just e) = Just (If c t e)
    condicional _ _ _ = Nothing
desugar (CondS clausulas alternativa) = desugarCond clausulas alternativa
desugar (LetRecS nombre definicion cuerpo) =
  desugar (LetS nombre (AppS (IdS "Y") [FunS [nombre] definicion]) cuerpo)

anidaLets :: [(Nombre, SASA)] -> SASA -> SASA
anidaLets [] cuerpo = cuerpo
anidaLets ((nombre, valor):resto) cuerpo =
  LetS nombre valor (anidaLets resto cuerpo)

desugarLista :: [SASA] -> Maybe [ASA]
desugarLista [] = Just []
desugarLista (x:xs) = agrega (desugar x) (desugarLista xs)
  where
    agrega (Just x') (Just xs') = Just (x':xs')
    agrega _ _ = Nothing

-- RETO 4: evaluacion perezosa con alcance estatico ------------------------

-- Busca la asociacion mas reciente sin exigir su contenido.
lookupEnv :: Nombre -> Env -> Maybe Value
lookupEnv _ [] = Nothing
lookupEnv nombre ((identificador, valor):resto)
  | nombre == identificador = Just valor
  | otherwise = lookupEnv nombre resto

-- Exige una cerradura de expresion usando el ambiente guardado. Si al
-- evaluarla se obtiene otra ExprV, continua hasta producir otro valor.
strict :: Value -> Maybe Value

-- Semantica de paso grande con alcance estatico y evaluacion perezosa.
--
-- * Id devuelve directamente la asociacion encontrada.
-- * Fun produce ClosureV con el ambiente de definicion.
-- * App exige la posicion de funcion, pero liga el argumento como
--   ExprV argumento ambienteDeLaLlamada.
-- * Add, Sub y Not exigen sus operandos.
-- * If exige solamente la condicion y evalua una sola rama.
--
-- La resta sobre naturales permanece truncada en cero.
bigStep :: Env -> ASA -> Maybe Value
