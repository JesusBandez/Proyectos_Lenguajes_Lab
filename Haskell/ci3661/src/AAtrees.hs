{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module AAtrees
    (
    ) where

import Prelude hiding (lookup,map)
import Debug.Trace (traceEvent)

-- | Un arbol AA que contiene una clave (k) y un valor asociado a esa clave (a).
data AA k a = Empty | Node { lvl :: Int, key :: k, val :: a, lAA :: AA k a, rAA :: AA k a}

instance (Ord k, Semigroup (AA k v)) => Monoid (AA k v) where
    mempty  = empty
    mappend = unionAA

instance (Show k, Show a) => Show (AA k a) where
  showsPrec d m  = showParen (d > 10) $
    shows (toList m)

instance Functor (AA k) where
  fmap f m  = map f m

instance Foldable (AA k) where
  foldMap _f Empty = mempty
  foldMap f (Node _n _kv v l r) = foldMap f l `mappend` f v `mappend` foldMap f r

-- | Convierte un arbol AA en una lista de tuplas donde el primer elemento es la clave y el segundo el valor asociado.
-- | toList (Node 1 1 "a" Empty (Node 2 2 "b" Empty Empty)) = [(1, "a"), (2, "b")]
toList :: AA k a -> [(k,a)]
toList = go []
  where
    go list Empty             = list
    go list (Node _ kx x l r) = go ((\k x xs -> (k,x):xs) kx x (go list r)) l

-- | Convierte una lista de tuplas donde el primer elemento es la clave y el segundo el valor asociado en un arbol AA.
-- | fromList [(1, "a"), (2, "b")] = (Node 1 1 "a" Empty (Node 1 2 "b" Empty Empty))
fromList :: Ord k => [(k,a)] -> AA k a
fromList xs =  (split . skew)  (go empty xs)
  where
    go tree []     = tree
    go tree (x:xs) = tree `seq` go (ins tree x) xs

    ins t (kx,x) = insert kx x t

-- | Une dos arboles AA en un solo arbol AA.
-- | unionAA (Node 1 1 "a" Empty (Node 2 2 "b" Empty Empty)) (Node 1 2 "A" Empty (Node 2 3 "C" Empty Empty)) = (Node 1 1 "a" Empty (Node 2 2 "b" Empty (Node 1 3 "C" Empty Empty)))
unionAA :: (Ord k) => AA k a -> AA k a -> AA k a
unionAA x y = fromList (toList x `union` toList y)

-- | Une dos listas de tuplas en una sola. 
-- | Si las listas contienen una tupla con la misma clave, se le asigna el valor del arbol que se pasa primero como argumento.
-- | union [(1, "a"), (2, "b")] [(2, "A"), (3, "C")] = [(1, "a"), (2, "b"), (3, "C")]
union :: (Ord ka) => [(ka, a)] -> [(ka, a)] -> [(ka, a)]
union = unionBy sameKey

-- | Version de union que acepta el metodo de comparacion como parametro.
unionBy :: (Ord ka) => ((ka, a) -> (ka, a) -> Bool) -> [(ka, a)] -> [(ka, a)] -> [(ka, a)]
unionBy eq xs ys =  xs ++ foldl (flip (deleteBy eq)) (nubBy eq ys) xs

-- | Si alguna tupla de la lista es igual a la tupla pasada como argumnento, entonces se elimina esa tupla de la lista. 
-- | deleteBy sameKey (2, "aa") [(1, "a"), (2, "aa"), (3, "aaa")] = [(1, "a"), (3, "aaa")]
deleteBy                :: (Ord ka) => ((ka, a) -> (ka, a) -> Bool) -> (ka, a) -> [(ka, a)] -> [(ka, a)]
deleteBy _  _ []        = []
deleteBy eq (kx, x) ((ky,y):ys)    = if (kx, x) `eq` (ky,y) then ys else (ky,y) : deleteBy eq (kx, x) ys

-- | Dentro de la misma lista, no pueden haber tuplas con la misma clave. Deja la primera que encuentre en la lista.
-- | nubBy sameKey [(1, "a"), (2, "aa"), (2, "aaa"), (2, "aaaa")] = [(1, "a"), (2, "aa")]
nubBy                   :: ((ka, a) -> (ka, a) -> Bool) -> [(ka, a)] -> [(ka, a)]
nubBy eq []             =  []
nubBy eq ((kx, x) :xs)         =  (kx, x) : nubBy eq (filter (\ (ky, y) -> not (eq (kx, x) (ky, y))) xs)

-- | Compara dos tuplas y retorna true si el primer elemento es el mismo, false en el caso contrario.
-- | sameKey (1, "a") (1, "aa") = True
sameKey :: (Ord ka) => (ka, a) -> (ka, a) -> Bool
sameKey (kx, x) (ky, y) = kx == ky

-- | Aplica una funcion pasada como argumento a todos los valores en los nodos de un arbol AA.
-- | map (++"a") [(1, "a"), (2, "b"), (3, "c")] = [(1, "aa"), (2, "ba"), (3, "ca")]
map :: (a -> b) -> AA k a -> AA k b
map f = go
  where
      go Empty = Empty
      go (Node n kv v l r) = Node n kv ((\_ x -> f x) kv v) (go l) (go r)

{-------------------------------------------------------------------------------------------------------------
FUNCIONES
-------------------------------------------------------------------------------------------------------------}

-- | Crea un arbol AA vacio
empty :: AA k a
empty = Empty

-- | Verifica si un nodo esta vacio
-- | isEmpty Empty = True
isEmpty :: AA k a -> Bool
isEmpty Empty      = True
isEmpty Node {} = False

-- | Rebalancea (evita que se rompan los invariantes del arbol AA) un nodo recibido como argumento que representa un arbol AA y lo retorna.
-- | El balanceo lo hace realizando una rotación a la derecha para sustituir un subarbol a la izquierda por otro a la derecha
-- | skew (Node 2 4 "d" (Node 2 2 "b" (Node 1 1 "a" Empty Empty) (Node 1 3 "c" Empty Empty)) (Node 2 5 "e" (Node 1 6 "f" Empty Empty) (Node 1 7 "g" Empty Empty))) 
-- | = (Node 2 2 "b" (Node 1 1 "a" Empty Empty) (Node 2 4 "d" (Node 1 3 "c" Empty Empty)  (Node 2 5 "e" (Node 1 6 "f" Empty Empty) (Node 1 7 "g" Empty Empty)))
skew :: AA k a -> AA k a
skew Empty = Empty
skew t@(Node n kv v Empty r) = Node n kv v Empty r
skew (Node n kv v (Node ln lkv lv ll lr) r )
    | ln == n = Node ln lkv lv ll (Node n kv v lr r)
skew t = t

-- | Rebalancea (evita que se rompan los invariantes del arbol AA) un nodo recibido como argumento que representa un arbol AA y lo retorna.
-- | El balanceo lo hace realizando una rotación a la izquierda y aumentando de nivel para sustituir un subarbol con dos o más enlaces horizontales consecutivos a la derecha por otro con dos enlaces horizontales consecutivos menos.
-- | split (Node 1 1 "a" Empty (Node 1 2 "b" Empty (Node 1 3 "c" Empty Empty))) = (Node 2 2 "b" (Node 1 1 "a" Empty (Node 1 3 "c" Empty Empty) (Node 1 3 "c" Empty Empty)))
split :: AA k a -> AA k a
split Empty = Empty
split t@(Node n kv v l Empty) = t
split t@(Node n kv v l (Node rn rkv rv rl Empty)) = t
split (Node n kv v l (Node rn rkv rv rl rrnode@(Node rrn _ _ _ _)))
    | n == rrn = Node (rn + 1) rkv rv (Node n kv v l rl) rrnode
split t = t

-- | Inserta un nuevo nodo en el arbol si el mismo tiene una clave diferente a las almacenadas y retorna el arbol resultante balanceado.
-- | insert 3 "c" (Node 2 4 "a" Empty (Node 1 5 "b" Empty Empty)) = (Node 2 4 "a" (Node 1 3 "c" Empty Empty) (Node 1 5 "b" Empty Empty))
insert :: (Ord k) => k -> a -> AA k a -> AA k a
insert kx x Empty = Node 1 kx x Empty Empty
insert kx x t@(Node n kv v l r) | kx < kv = (split . skew)  (Node n kv v (insert kx x l) r)
                                | kx > kv = (split . skew)  (Node n kv v l (insert kx x r))
insert kx x t = t

-- | Busca un valor por la clave en un arbol AA y lo retorna (si lo logra encontrar, si no, retorna Nothing)
-- | lookup 5 (Node 2 4 "a" (Node 1 3 "c" Empty Empty) (Node 1 5 "b" Empty Empty)) = Just "b"
lookup :: (Ord k) => k -> AA k a -> Maybe a
lookup k = k `seq` go
  where
    go Empty = Nothing
    go (Node _ kv v l r) =
        case compare k kv of
            LT -> go l
            GT -> go r
            EQ -> Just v

level :: AA k a -> Maybe Int
level Empty = Nothing
level (Node n _ _ _ _) = Just n

{-------------------------------------------------------------------------------------------------------------
INVARIANTES
-------------------------------------------------------------------------------------------------------------}

data AAValid a = Valid | LeafNodeNoOne a | LeftChildIsNotOneLess a | RightChildNotOneLessOrEqual a | RightGrandChildNotStrictlyLess a | NoTwoChildren a 

-- instance Show (AAValid a) where 
--     show (LeftChildIsNotOneLess a) = "Small"

-- showTree :: (Show k, Show a) => AAValid (AA k a) -> String
-- showTree Valid = "Valid"
-- showTree (LeafNodeNoOne (Node n kv v l r)) = "LeafNodeNoOne (Node " ++ show n ++ " " ++ show kv ++ " " ++ show v ++ " " ++ 
 
isValid :: AAValid (AA k a) -> Bool
isValid Valid      = True
isValid _ = False

-- | Filtra los nodos que retornaron validos de la lista y deja solo los que rompienron invariantes
checkInvariant :: AA k a -> [AAValid (AA k a)]
checkInvariant t = filter (not . isValid) (checkInvariantTree t)

-- | Chequea si un arbol cumple todos los invariantes y retorna los resultados por nodo evaluado en una lista
checkInvariantTree :: AA k a -> [AAValid (AA k a)]
checkInvariantTree Empty = [checkInvariantNode Empty]
checkInvariantTree (Node n kv v l r) = (checkInvariantNode (Node n kv v l r) : checkInvariant l) ++ checkInvariant r

-- | Chequea si un nodo que representa un arbol rompe algún invariante
checkInvariantNode :: AA k a -> AAValid (AA k a)
checkInvariantNode Empty = Valid
checkInvariantNode t@(Node n _ _ Empty Empty)  | n == 1 = Valid 
                                                | otherwise = LeafNodeNoOne t 
checkInvariantNode t@(Node n _ _ Empty _)  | n > 1 = NoTwoChildren t
                                        | otherwise = Valid 
checkInvariantNode t@(Node n _ _ _ Empty)  | n > 1 = NoTwoChildren t
                                           | otherwise = Valid 
checkInvariantNode t@(Node n _ _ lnode@(Node ln _ _ _ll _lr)  rnode@(Node rn _ _ _rl rr))   | rn /= (n - 1) && rn /= n = RightChildNotOneLessOrEqual t
                                                                                            | ln /= (n - 1) = LeftChildIsNotOneLess t
                                                                                            | not (isEmpty rr) && lvl rr < n = RightGrandChildNotStrictlyLess t
                                                                                            | otherwise = Valid



-- (Node 2 1 "a"  (Node 1 5 "e" Empty Empty) (Node 3 2 "b" (Node 2 3 "c" Empty Empty) (Node 2 4 "d" (Node 1 6 "f" Empty Empty) (Node 2 7 "h" (Node 1 8 "i" Empty Empty) (Node 1 9 "j" Empty Empty)))))