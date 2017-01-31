module Chinchon where

import Data.List (sort, permutations, tails, (\\), sortBy, union)
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

-- Mezclar cartas, sacado de un repositorio para mayor eficiencia
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
repartirCartaAJugador m (Jugador nombre mano s ss) = let (carta, m') = repartir m
                                                 in (m', Jugador nombre (carta:mano) s ss)

repartirNCartasAJugador :: Int -> Mazo -> Jugador -> (Mazo, Jugador)
repartirNCartasAJugador n m j
    | n > length m  = error "No hay cartas suficientes"
    | n < 1         = error "Debes repartir por lo menos 1 carta"
    | n == 1        = repartirCartaAJugador m j
    | otherwise     = repartirNCartasAJugador (n - 1) m' j' 
        where (m', j') = repartirCartaAJugador m j


tomarCartaDesconocida :: EstadoDeJuego -> EstadoDeJuego
tomarCartaDesconocida (jugador, computadora, mazo, pilaDescartadas) = (jugador', computadora, mazo', pilaDescartadas)
                        where (mazo', jugador') = repartirCartaAJugador mazo jugador

tomarUltimaCartaDescartada :: EstadoDeJuego -> EstadoDeJuego
tomarUltimaCartaDescartada (jugador, computadora, mazo, pilaDescartadas) = (jugador', computadora, mazo, pilaDescartadas')
                        where (pilaDescartadas', jugador') = repartirCartaAJugador pilaDescartadas jugador

ultimaCartaDescartada :: [Carta] -> Carta
ultimaCartaDescartada pilaDescartadas = head pilaDescartadas
                        
mostrarUltimaCartaDescartada :: EstadoDeJuego -> String
mostrarUltimaCartaDescartada (_, _, _, pilaDescartadas) = show $ head pilaDescartadas

mostrarMano :: EstadoDeJuego -> String
mostrarMano ((Jugador _ mano _ _), _, _, _) = show mano

descartar :: Descartar
descartar mano n mazo = let c = mano !! (n - 1) in (mano \\ [c], c:mazo)


--VER ESTOS --

combinaciones :: Int -> [a] -> [[a]]
combinaciones 0 _  = [[]]
combinaciones n xs = [ y:ys | y:xs' <- tails xs, ys <- combinaciones (n-1) xs']

--VER ESTOS

--combinacionesDeMano :: Mano -> [Mano]
--combinacionesDeMano = combinaciones 7

esBuena :: [Carta] -> Bool
esBuena cartas = cartasEnSecuencia cartas || cartasEnGrupo cartas

--VER ESTOS 
puedeGanar :: Jugador -> Bool
puedeGanar (Jugador _ mano _ _) = puedeGanarMenosDiez mano || puedeGanarSobraUna mano

puedeGanarMenosDiez :: [Carta] -> Bool
puedeGanarMenosDiez mano =  do
--										let combinacionesDe7 = combinacionesDeMano mano
                                        let buenasCombinaciones mano' = [ (cuatro, tres) | cuatro <- combinaciones 4 mano', let tres = mano' \\ cuatro, esBuena cuatro, esBuena tres]
                                        let  existeSolucion  = \m -> (length $ buenasCombinaciones m) >= 1
                                        any existeSolucion [mano]


puedeGanarSobraUna :: [Carta] -> Bool
puedeGanarSobraUna mano =  do
                                        let buenasCombinaciones mano' = [ (tres, tres'') | tres <- combinaciones 3 mano',let tres' = mano' \\ tres, tres'' <- combinaciones 3 tres', esBuena tres, esBuena tres'']
                                        let  existeSolucion  = \m -> (length $ buenasCombinaciones m) >= 1
                                        any existeSolucion [mano]
                                        
-- Obtener por palo
obtenerPorPalo :: Palo -> [Carta] -> [Carta]
obtenerPorPalo paloParam cartas = filter (\carta -> palo carta == paloParam) cartas

-- supone que mandas la cantidad N de cartas en el array

tieneNSuc :: [Carta] -> Int -> Bool
tieneNSuc [] _ = False
tieneNSuc _ 0 = False
tieneNSuc (x:xs) 1 = True
tieneNSuc cartas n | n > length cartas = False
tieneNSuc (x:y:zs) n | (valor y == succ (valor x)) && (palo y == palo x) =  tieneNSuc (y:zs) (n-1)
tieneNSuc (x:y:zs) n | valor y ==  valor x = False
tieneNSuc (x:y:zs) n | (valor y < valor x) && (palo y == palo x) = True


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

esSemiSegura :: [Carta] -> Bool
esSemiSegura cartas = esSemiSeguraPalo cartas || esSemiSeguraNumero cartas

esSemiSeguraPalo :: [Carta] -> Bool
esSemiSeguraPalo cartas = do
	let cartas' = ordenarCartasNumero cartas
	tieneNSuc cartas' 2 

esSemiSeguraNumero :: [Carta] -> Bool
esSemiSeguraNumero [(Carta n1 _), (Carta n2 _)] = n1 == n2


removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
					| otherwise = y : removeItem x ys

jugadaComputadora :: EstadoDeJuego -> EstadoDeJuego
jugadaComputadora estado@(jugador, (Jugador nombre mano seguras ss), mazo, pilaDescartadas) = do
-- 	VER SI LA DE DESCARTADA SIRVE
    let cDescartada = (ultimaCartaDescartada pilaDescartadas)
	let mano' = [cDescartada] ++ mano
	let pilaDescartadas' = tail pilaDescartadas
	if length [cuatro | cuatro <- combinaciones 4 mano'\\(combinaciones 3 seguras), esBuena cuatro] >=1
	then do
		let seguras' = seguras ++ [cDescartada]
		let computadora' = (Jugador nombre mano' seguras' ss)
		let sobrantes = [carta | carta <- mano', carta `notElem` (seguras' `union` ss)] -- TODAS LAS QUE NO ESTAN EN SEGURAS Y SS
		if length sobrantes > 0
		then do
			let cartasOrdenadas = ordenarCartasNumero sobrantes
			let descartada = last (cartasOrdenadas) --SACAR UNA (EN LO POSIBLE LA MAS GRANDE)
			let mano'' = init (cartasOrdenadas) ++ seguras' ++ ss  -- y poner el resto en mano''
			let pilaDescartadas'' = [descartada] ++ pilaDescartadas'
			let computadora'' = (Jugador nombre mano'' seguras' ss)
			(jugador, computadora'', mazo, pilaDescartadas'')
		else do
			let cartasOrdenadas = ordenarCartasNumero ss
			let descartada = last (cartasOrdenadas)
			let ssComb = [dos | dos <- combinaciones 2 ss, esSemiSegura dos && descartada `elem` dos]
			let conjEscalera = [cartas | cartas <- ssComb, esSemiSeguraPalo cartas]
			let conjNumeros = [cartas | cartas <- ssComb, esSemiSeguraNumero cartas]
			let aEliminar = if length conjEscalera >=1 then head conjEscalera else head conjNumeros
			let [aDejar] = [carta | carta <- aEliminar, not (carta == descartada)]
			let ss' = [carta | carta <- cartasOrdenadas, not (carta == descartada) && not (carta == aDejar) ] 
			let pilaDescartadas'' = [descartada] ++ pilaDescartadas'
			let mano'' = seguras' ++ ss' ++ [aDejar] --aca no se repetirian??
			let computadora'' = (Jugador nombre mano'' seguras' ss')
			(jugador, computadora'', mazo, pilaDescartadas'')
					
--	else (jugador, (Jugador "AA" mano seguras ss), mazo, tail pilaDescartadas)
	else do
	let combPosibles = [tres | tres <- combinaciones 3 mano'\\(combinaciones 2 ss), esBuena tres && cDescartada `elem` tres]
	if length combPosibles >=1
	then do
		let seguras' = seguras ++ last combPosibles
		let ss' = [carta | carta <- ss, carta `notElem` seguras']
		let computadora' = (Jugador nombre mano' seguras' ss')
		(jugador, computadora', mazo, pilaDescartadas')
--		let manoAux = [carta | carta <- mano', carta `notElem` (seguras' `union` ss')] -- TODAS LAS QUE NO ESTAN EN SEGURAS Y SS
--		if length manoAux > 0
--		then do
--			let cartasOrdenadas = ordenarCartasNumero manoAux
--			let descartada = last (cartasOrdenadas) --SACAR UNA (EN LO POSIBLE LA MAS GRANDE)
--			let mano'' = init (cartasOrdenadas) ++ seguras' ++ ss  -- y poner el resto en mano''
--			let pilaDescartadas'' = [descartada] ++ pilaDescartadas'
--			let computadora'' = (Jugador nombre mano'' seguras' ss)
--			(jugador, computadora'', mazo, pilaDescartadas'')
--		else do
--			let cartasOrdenadas = ordenarCartasNumero ss
--			let descartada = last (cartasOrdenadas)
--			let ssComb = [dos | dos <- combinaciones 2 ss, esSemiSegura dos && descartada `elem` dos]
--			let conjEscalera = [cartas | cartas <- ssComb, esSemiSeguraPalo cartas]
--			let conjNumeros = [cartas | cartas <- ssComb, esSemiSeguraNumero cartas]
--			let aEliminar = if length conjEscalera >=1 then head conjEscalera else head conjNumeros
--			let [aDejar] = [carta | carta <- aEliminar, not (carta == descartada)]
--			let ss' = [carta | carta <- cartasOrdenadas, not (carta == descartada) && not (carta == aDejar) ] 
--			let pilaDescartadas'' = [descartada] ++ pilaDescartadas'
--			let mano'' = seguras' ++ ss' ++ [aDejar] --aca no se repetirian??
--			let computadora'' = (Jugador nombre mano'' seguras' ss')
--			(jugador, computadora'', mazo, pilaDescartadas'')
	-- SI HAY, AGREGAR LA CARTA A SEGURAS
	else (jugador, (Jugador "AA" mano seguras ss), mazo, tail pilaDescartadas)
	
	
---- SINO SIRVE, SEGUIR CON LA DESCONOCIDA
--	(mazo', computadora') = repartirCartaAJugador mazo computadora
--	
----DESCARTAR CARTA QUE NO SIRVE
--
--	return (jugador, computadora'', mazo', pilaDescartadas)
    
-- QUE SIRVA SIGINIFICA QUE PUEDE FORMAR 4 EN ESCALERA, 3 EN ESCALERA, 4 EN GRUPO o 3 EN GRUPO.
