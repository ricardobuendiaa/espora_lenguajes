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
  deriving (Eq, Show)

data Value
  = NumV Int
  | BooleanV Bool
  | ClosureV Nombre ASA Env
  deriving (Eq, Show)

type Env = [(Nombre, Value)]

-- RETO 1: desazucarado ----------------------------------------------------

-- Convierte una lista no vacia de parametros distintos en funciones
-- unarias anidadas. El primer parametro queda en la funcion exterior.
curryFun :: [Nombre] -> ASA -> Maybe ASA
curryFun [] _ = Nothing
curryFun parametros cuerpo =
  if repetidos parametros
    then Nothing
    else Just (currifica parametros cuerpo)
  where
    currifica [x] c = Fun x c
    currifica (x : xs) c = Fun x (currifica xs c)
    currifica [] x = x

    repetidos [] = False
    repetidos (x:xs) = if x `elem` xs then True else repetidos xs

-- Convierte una aplicacion con uno o mas argumentos en aplicaciones unarias
-- asociadas por la izquierda.
curryApp :: ASA -> [ASA] -> Maybe ASA
curryApp _ [] = Nothing
curryApp funcion argumentos = Just (aplica funcion argumentos)
  where
    aplica f [] = f
    aplica f (a:as) = aplica (App f a) as

-- Convierte dos o mas operandos en operaciones binarias asociadas por la
-- izquierda. El constructor recibido sera Add o Sub.
binaryOp :: (ASA -> ASA -> ASA) -> [ASA] -> Maybe ASA
binaryOp _ [] = Nothing
binaryOp _ [_] = Nothing
binaryOp op (x:y:xs ) = Just (combina (op x y) xs)
  where
    combina acumulado [] = acumulado
    combina acumulado (e: es) = combina (op acumulado e) es


-- Convierte las ligaduras de let* en let anidados y despues elimina cada let
-- mediante LetS x e1 e2 ==> App (Fun x e2') e1'. La primera ligadura debe
-- quedar en el let exterior para que las siguientes puedan usarla.
desugar :: SASA -> Maybe ASA
desugar (IdS i) = Just (Id i)
desugar (NumS n) = Just (Num n)
desugar (BooleanS b) = Just (Boolean b)
desugar (NotS e) = do
  e' <- desugar e
  Just (Not e')
desugar (AddS op) =
  case desugarLista op of
    Nothing -> Nothing
    Just ops -> binaryOp Add ops
desugar (SubS op) =
  case desugarLista op of
    Nothing -> Nothing
    Just ops -> binaryOp Sub ops
desugar (FunS parametros cuerpo) = 
  case desugar cuerpo of
    Nothing -> Nothing
    Just cuerpo' -> curryFun parametros cuerpo'
desugar (AppS funcion argumentos) =
  case desugar funcion of
    Nothing -> Nothing
    Just funcion' -> case desugarLista argumentos of
      Nothing -> Nothing
      Just argumentos' -> curryApp funcion' argumentos'
desugar (LetS nombre valor cuerpo) = 
  case desugar valor of
    Nothing -> Nothing
    Just valor' -> case desugar cuerpo of
      Nothing -> Nothing
      Just cuerpo' -> Just (App (Fun nombre cuerpo') valor')
desugar (LetStarS ligaduras cuerpo) =
  desugar (anidaLets ligaduras cuerpo)

anidaLets :: [(Nombre, SASA)] -> SASA -> SASA
anidaLets [] cuerpo = cuerpo
anidaLets ((nombre, valor): resto) cuerpo =
  LetS nombre valor (anidaLets resto cuerpo)

desugarLista :: [SASA] -> Maybe [ASA]
desugarLista [] = Just []
desugarLista (x: xs) = 
  case desugar x of
    Nothing -> Nothing
    Just x' -> case desugarLista xs of
      Nothing -> Nothing
      Just xs' -> Just (x':xs')

-- RETO 2: evaluacion con cerraduras ---------------------------------------

-- Busca la asociacion mas reciente de un identificador.
lookupEnv :: Nombre -> Env -> Maybe Value
lookupEnv _ [] = Nothing
lookupEnv nombre ((id, valor): resto) =
  if nombre == id
    then Just valor
    else lookupEnv nombre resto

-- Evalua con alcance estatico. Fun produce una cerradura con el ambiente
-- actual. App evalua primero la posicion de funcion, despues el argumento y
-- por ultimo el cuerpo en el ambiente guardado por la cerradura.
-- La aplicacion es ansiosa: el argumento se exige aunque el cuerpo no lo use.
-- Conserva la resta truncada y la convencion de que todo numero cuenta como
-- verdadero cuando aparece como operando de Not.
bigStep :: Env -> ASA -> Maybe Value
bigStep env (Id nombre) = lookupEnv nombre env

bigStep env (Num n) = Just (NumV n)

bigStep env (Boolean b) = Just (BooleanV b)

bigStep env (Add izquierda derecha) =
  case bigStep env izquierda of
    Just (NumV n) ->
      case bigStep env derecha of
        Just (NumV m) -> Just (NumV (n + m))
        _ -> Nothing
    _ -> Nothing

bigStep env (Sub izquierda derecha) =
  case bigStep env izquierda of
    Just (NumV n) ->
      case bigStep env derecha of
        Just (NumV m) -> Just (NumV (max 0 (n - m)))
        _ -> Nothing
    _ -> Nothing

bigStep env (Not expresion) =
  case bigStep env expresion of
    Just (BooleanV b) -> Just (BooleanV (not b))
    Just (NumV _) -> Just (BooleanV False)
    _ -> Nothing

bigStep env (Fun parametro cuerpo) =
  Just (ClosureV parametro cuerpo env)

bigStep env (App funcion argumento) =
  case bigStep env funcion of
    Just valorFuncion ->
      case bigStep env argumento of
        Just valorArgumento ->
          case valorFuncion of
            ClosureV parametro cuerpo ambienteDefinicion ->
              bigStep ((parametro, valorArgumento) : ambienteDefinicion) cuerpo
            _ -> Nothing
        Nothing -> Nothing
    Nothing -> Nothing
