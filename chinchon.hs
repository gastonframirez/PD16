module Chinchon where

import Data.List (sort, permutations, tails, (\\), sortBy)
import Data.Ord (comparing)
import System.Random
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Data.STRef

--Creacion mazo de 48 cartas con simbolos de poker pero sin los 4 literales (A, J , Q, K) 
--Se ordenan de menor a mayor rango de la siguiente forma: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 y 12 

data Palo =  Treboles | Diamantes | Corazones | Picas deriving (Eq, Ord, Enum)
data Valor = Uno | Dos | Tres | Cuatro | Cinco | Seis | Siete | Ocho | Nueve | Diez | Once | Doce deriving (Show, Eq, Ord, Enum)
data Carta = Carta {valor :: Valor, palo :: Palo} deriving (Eq)

type Mazo = [Carta]
type Mano = [Carta]
type Seguras = [Carta]
type SemiSeguras = [Carta]

type Repartir = Mazo -> (Carta, Mazo)
data Jugador = Jugador { nombre :: String, mano :: Mano, seguras :: Seguras, semiSeguras ::SemiSeguras} deriving (Show)

type EstadoDeJuego = (Jugador, Jugador, Mazo, Mazo)
type Descartar = Mano -> Int -> Mazo -> (Mano, Mazo)

--Se asignan simbolo ASCII a cada palo 
instance Show Palo where
    show Picas = "♠"
    show Corazones = "♥"
    show Diamantes = "♦"
    show Treboles = "♣"

--Se asignan numeros, literales y signos a cada carta depende su valor
instance Show Carta where
    show (Carta Uno palo)   = "1" ++ show palo
    show (Carta Dos palo)   = "2" ++ show palo
    show (Carta Tres palo) = "3" ++ show palo
    show (Carta Cuatro palo)  = "4" ++ show palo
    show (Carta Cinco palo)  = "5" ++ show palo
    show (Carta Seis palo)   = "6" ++ show palo
    show (Carta Siete palo) = "7" ++ show palo
    show (Carta Ocho palo) = "8" ++ show palo
    show (Carta Nueve palo)  = "9" ++ show palo
    show (Carta Diez palo)   = "10" ++ show palo
    show (Carta Once palo)  = "11" ++ show palo
    show (Carta Doce palo) = "12" ++ show palo

nuevoMazo :: Mazo
nuevoMazo = [Carta n p | p <- [Treboles .. ], n <- [Uno .. ]]


nuevoJugador :: String -> Jugador
nuevoJugador nombre = Jugador nombre [] [] []

-- Funcion para mezclar cartas, encontrada en otro github porque no sabiamos como hacerla eficiente
shuffle' :: [a] -> StdGen -> ([a],StdGen)
shuffle' xs gen = runST (do
				g <- newSTRef gen
				let randomRST lohi = do
							(a,s') <- liftM (randomR lohi) (readSTRef g)
							writeSTRef g s'
							return a
				ar <- newArray n xs
				xs' <- forM [1..n] $ \i -> do
								j <- randomRST (i,n)
								vi <- readArray ar i
								vj <- readArray ar j
								writeArray ar j vi
								return vj
				gen' <- readSTRef g
				return (xs',gen'))
	where
		n = length xs
		newArray :: Int -> [a] -> ST s (STArray s Int a)
		newArray n xs =  newListArray (1,n) xs

shuffleIO :: [a] -> IO [a]
shuffleIO xs = getStdRandom (shuffle' xs)

-- Repartir cartas
repartir :: Repartir
repartir [] = error "Mazo Vacio"
repartir (x:xs) = (x, xs)

repartirCartaAJugador :: Mazo -> Jugador -> (Mazo, Jugador)
repartirCartaAJugador [] _ = error "Mazo Vacio"
repartirCartaAJugador m (Jugador nombre mano [] []) = let (carta, m') = repartir m
                                                 in (m', Jugador nombre (carta:mano) [] [])

repartirNCartasAJugador :: Int -> Mazo -> Jugador -> (Mazo, Jugador)
repartirNCartasAJugador n m j
    | n > length m  = error "No hay cartas suficientes"
    | n < 1         = error "Debes repartir por lo menos 1 carta"
    | n == 1        = repartirCartaAJugador m j
    | otherwise     = repartirNCartasAJugador (n - 1) m' j' 
        where (m', j') = repartirCartaAJugador m j
        
--jugadaCompu :: EstadoDeJuego -> EstadoDeJuego
-- buscar escalera (mismo palo) 1º 4, 2º 3 (para ponerlo en seguras)

-- buscar mismo numero 1º 4, 2º 3 (para ponerlo en seguras)

-- igual que lo anterior, pero para 2 cartas




-- Obtener por palo
obtenerPorPalo :: Palo -> [Carta] -> [Carta]
obtenerPorPalo paloParam cartas = filter (\carta -> palo carta == paloParam) cartas

-- supone que todas son del mismo Palo
tieneNSuc :: [Carta] -> Int -> Bool
tieneNSuc [] _ = False
tieneNSuc _ 0 = False
tieneNSuc (x:xs) 1 = True
tieneNSuc l b | b > length l = False
tieneNSuc (x:y:zs) b | valor y == succ (valor x) =  tieneNSuc (y:zs) (b-1)
tieneNSuc (x:y:zs) b | valor y ==  valor x = False
tieneNSuc (x:y:zs) b | valor y < valor x = True

-- supone todas las cartas del mismo Palo
esChinchon :: [Carta] -> Bool
esChinchon cartas =	tieneNSuc cartas 7


-- ordernar cartas (supone del mismo Palo)
ordenarCartasNumero :: [Carta] -> [Carta]
ordenarCartasNumero = sortBy (comparing valor)

--
ordenarCartasPalo :: [Carta] -> [Carta]
ordenarCartasPalo = sortBy (comparing palo)

-- ordena todas cartas 
ordenarTodasCartas :: [Carta] -> [Carta]
ordenarTodasCartas cartas = ordenarCartasPalo (ordenarCartasNumero cartas)

-- igual que lo anterior, pero para 2 cartas
cartasEnGrupo :: [Carta] -> Bool
cartasEnGrupo [(Carta n1 _), (Carta n2 _), (Carta n3 _)] = n1 == n2 && n1 == n3
cartasEnGrupo [(Carta n1 _), (Carta n2 _), (Carta n3 _), (Carta n4 _)] = n1 == n2 && n1 == n3 && n1 == n4
cartasEnGrupo _ = False


-- Secuencia
cartasEnSecuencia :: [Carta] -> Bool
cartasEnSecuencia [(Carta n1 p1), (Carta n2 p2), (Carta n3 p3)] = mismoPalo && enSecuencia
        where 
            mismoPalo = p1 == p2 && p1 == p3
            enSecuencia = ordenado !! 0 + 1 == ordenado !! 1 && ordenado !! 1 + 1 == ordenado !! 2
            ordenado = map fromEnum $ sort [n1, n2, n3]
cartasEnSecuencia [(Carta n1 p1), (Carta n2 p2), (Carta n3 p3), (Carta n4 p4)] = mismoPalo && enSecuencia
        where 
            mismoPalo = p1 == p2 && p1 == p3 && p1 == p4
            enSecuencia = ordenado !! 0 + 1 == ordenado !! 1 && ordenado !! 1 + 1 == ordenado !! 2 && ordenado !! 2 + 1 == ordenado !! 3
            ordenado = map fromEnum $ sort [n1, n2, n3,n4]
cartasEnSecuencia _ = False

