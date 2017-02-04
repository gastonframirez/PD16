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

obtenerValorNumerico :: Carta -> Int
obtenerValorNumerico (Carta Uno _) = 1
obtenerValorNumerico (Carta Dos _) = 2
obtenerValorNumerico (Carta Tres _) = 3 
obtenerValorNumerico (Carta Cuatro _) = 4
obtenerValorNumerico (Carta Cinco _) = 5
obtenerValorNumerico (Carta Seis _) = 6
obtenerValorNumerico (Carta Siete _) = 7
obtenerValorNumerico (Carta Ocho _) = 8 
obtenerValorNumerico (Carta Nueve _) = 9
obtenerValorNumerico (Carta Diez _) = 10
obtenerValorNumerico (Carta Once _) = 11
obtenerValorNumerico (Carta Doce _) = 12


data Carta = Carta {valor :: Valor, palo :: Palo} deriving (Eq)

type Mazo = [Carta]
type Mano = [Carta]
type Seguras = [Carta]
type SemiSeguras = [Carta]

type Repartir = Mazo -> (Carta, Mazo)
data Jugador = Jugador { nombre :: String, mano :: Mano, seguras :: Seguras, semiSeguras ::SemiSeguras, puntos :: Int} deriving (Show)

type EstadoDeJuego = (Jugador, Jugador, Mazo, Mazo)
type Descartar = Mano -> Int -> Mazo -> (Mano, Mazo)

data CombinacionPuntaje = CombinacionPuntaje {combinacion :: [Carta], suma :: Int}

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
nuevoJugador nombre = Jugador nombre [] [] [] 0

-- Repartir cartas

repartir :: Repartir
repartir [] = error "Mazo Vacio"
repartir (x:xs) = (x, xs)

repartirCartaAJugador :: Mazo -> Jugador -> (Mazo, Jugador)
repartirCartaAJugador [] _ = error "Mazo Vacio"
repartirCartaAJugador m (Jugador nombre mano s ss puntos) = let (carta, m') = repartir m
                                                 in (m', Jugador nombre (carta:mano) s ss puntos)

repartirNCartasAJugador :: Int -> Mazo -> Jugador -> (Mazo, Jugador)
repartirNCartasAJugador n m j
    | n > length m  = error "No hay cartas suficientes"
    | n < 1         = error "Debes repartir por lo menos 1 carta"
    | n == 1        = repartirCartaAJugador m j
    | otherwise     = repartirNCartasAJugador (n - 1) m' j' 
        where (m', j') = repartirCartaAJugador m j



-- | Randomly shuffle a list without the IO Monad
--   /O(N)/

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


tomarCartaDesconocida :: EstadoDeJuego -> EstadoDeJuego
tomarCartaDesconocida (jugador, computadora, mazo, pilaDescartadas) = (jugador', computadora, mazo', pilaDescartadas)
                        where (mazo', jugador') = repartirCartaAJugador mazo jugador

cartaDesconocida ::  [Carta] -> Carta
cartaDesconocida mazoCartas = head mazoCartas

tomarUltimaCartaDescartada :: EstadoDeJuego -> EstadoDeJuego
tomarUltimaCartaDescartada (jugador, computadora, mazo, pilaDescartadas) = (jugador', computadora, mazo, pilaDescartadas')
                        where (pilaDescartadas', jugador') = repartirCartaAJugador pilaDescartadas jugador

ultimaCartaDescartada :: [Carta] -> Carta
ultimaCartaDescartada pilaDescartadas = head pilaDescartadas
                        
mostrarUltimaCartaDescartada :: EstadoDeJuego -> String
mostrarUltimaCartaDescartada (_, _, _, pilaDescartadas) = show $ head pilaDescartadas

mostrarMano :: EstadoDeJuego -> String
mostrarMano ((Jugador _ mano _ _ _), _, _, _) = show mano

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
puedeGanar (Jugador _ mano _ _ _) = puedeGanarMenosDiez mano || puedeGanarSobraUna mano

noPuedeCortar :: [Carta] -> Bool
noPuedeCortar mano = not (puedeGanar (Jugador "" mano [] [] 0))

puedeGanarComputadora :: Jugador -> Bool
puedeGanarComputadora (Jugador _ mano _ _ _) = puedeGanarMenosDiez mano || puedeGanarSobraUnaComputadora mano

puedeGanarMenosDiez :: [Carta] -> Bool
puedeGanarMenosDiez mano =  do
--                                        let combinacionesDe7 = combinacionesDeMano mano
                                        let buenasCombinaciones mano' = [ (cuatro, tres) | cuatro <- combinaciones 4 mano', let tres = mano' \\ cuatro, esBuena cuatro, esBuena tres]
                                        let  existeSolucion  = \m -> (length $ buenasCombinaciones m) >= 1
                                        any existeSolucion [mano]


puedeGanarSobraUna :: [Carta] -> Bool
puedeGanarSobraUna mano =  do
                                        let buenasCombinaciones mano' = [ (tres, tres'') | tres <- combinaciones 3 mano',let tres' = mano' \\ tres, tres'' <- combinaciones 3 tres', esBuena tres, esBuena tres'']
                                        let  existeSolucion  = \m -> (length $ buenasCombinaciones m) >= 1
                                        any existeSolucion [mano]
    
puedeGanarSobraUnaComputadora :: [Carta] -> Bool
puedeGanarSobraUnaComputadora mano =  do
                                        let buenasCombinaciones mano' = [ (tres, tres'') | tres <- combinaciones 3 mano',let tres' = mano' \\ tres, tres'' <- combinaciones 3 tres', esBuena tres, esBuena tres'']
                                        let restos mano' = concat [ (mano'\\tres)\\tres'' | tres <- combinaciones 3 mano',let tres' = mano' \\ tres, tres'' <- combinaciones 3 tres', esBuena tres, esBuena tres'']
                                        let restoMasBajo = head (ordenarCartasNumero (restos mano))
                                        let  existeSolucion  = \m -> ((length $ buenasCombinaciones m) >= 1 && (valor restoMasBajo) <= Dos )
                                        any existeSolucion [mano]                            
-- Obtener por palo
obtenerPorPalo :: Palo -> [Carta] -> [Carta]
obtenerPorPalo paloParam cartas = filter (\carta -> palo carta == paloParam) cartas

-- supone que mandas la cantidad N de cartas en el array y que estan ordenadas de menor a mayor
tieneNSuc :: [Carta] -> Int -> Bool
tieneNSuc [] _ = False
tieneNSuc _ 0 = False
tieneNSuc (x:xs) 1 = True
tieneNSuc (x:xs) _ | valor x == Doce = False
tieneNSuc cartas n | n > length cartas = False
tieneNSuc (x:y:zs) n | (valor y == succ (valor x)) && (palo y == palo x) =  tieneNSuc (y:zs) (n-1)
tieneNSuc (x:y:zs) n | valor y ==  valor x = False
tieneNSuc (x:y:zs) n | valor y > valor x = False
tieneNSuc (x:y:zs) n | (valor y < valor x) && (palo y == palo x) = True


-- supone todas las cartas del mismo Palo
esChinchon :: [Carta] -> Bool
esChinchon cartas =    tieneNSuc (ordenarCartasNumero cartas) 7


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
jugadaComputadora estado@(jugador, (Jugador nombre mano seguras ss puntos), mazo, pilaDescartadas) = do
--     VER SI LA DE DESCARTADA SIRVE
    let cLevantada = ultimaCartaDescartada pilaDescartadas
    let mano' = [cLevantada] ++ mano
    let pilaDescartadas' = tail pilaDescartadas
    let posibles = [cuatro | cuatro <- combinaciones 4 mano'\\(combinaciones 3 seguras), esBuena cuatro, cLevantada `elem` cuatro]
    if length posibles >=1
    then do
        let combElegida = if length [cartas | cartas <- posibles, cartasEnSecuencia cartas] >= 1 then last [cartas | cartas <- posibles, cartasEnSecuencia cartas] else last posibles
        let seguras' = removerDuplicados (seguras ++ combElegida)
        let ss' = [carta | carta <- ss, carta `notElem` seguras']
        let sobrantes = [carta | carta <- mano', carta `notElem` (seguras' `union` ss')] -- TODAS LAS QUE NO ESTAN EN SEGURAS Y SS
        if length sobrantes > 0
        then do
            descartarSobrante (jugador, (Jugador nombre mano' seguras' ss' puntos), mazo, pilaDescartadas') sobrantes
        else do
            descartarDeSemiSeguras (jugador, (Jugador nombre mano' seguras' ss' puntos), mazo, pilaDescartadas') cLevantada
                                
    else do
        let combPosibles = [tres | tres <- combinaciones 3 (mano'\\seguras)\\(combinaciones 2 ss), esBuena tres && cLevantada `elem` tres]
        if length combPosibles >=1
        then do
        -- ES ACA EL PROBLEMA, EL 9D SUPUESTAMENTE LE SIRVE, PERO EN VERDAD NO LE SERVIRIA PORQUE EL 8 YA ESTA USADO
            let combElegida = if length [cartas | cartas <- combPosibles, cartasEnSecuencia cartas] >= 1 then last [cartas | cartas <- combPosibles, cartasEnSecuencia cartas] else last combPosibles
            let seguras' = seguras ++ combElegida
            let ss' = [carta | carta <- ss, carta `notElem` seguras']
            let ssComb =  [carta | carta <- combinaciones 2 ss', esSemiSegura carta]
            let ss'' = removerDuplicados (concat ssComb)
            let sobrantes = [carta | carta <- mano', carta `notElem` (seguras' `union` ss'')] -- TODAS LAS QUE NO ESTAN EN SEGURAS Y SS

            if length sobrantes > 0
            then do
                descartarSobrante (jugador, (Jugador nombre mano' seguras' ss'' puntos), mazo, pilaDescartadas') sobrantes
            else do
                descartarDeSemiSeguras (jugador, (Jugador nombre mano' seguras' ss'' puntos), mazo, pilaDescartadas') cLevantada
        else cartaDesconocidaComputadora (estado) 
    
    
descartarSobrante :: EstadoDeJuego -> [Carta]-> EstadoDeJuego
descartarSobrante estado@(jugador, (Jugador nombre mano seguras ss puntos), mazo, pilaDescartadas) sobrantes = do
    let cartasOrdenadas = ordenarCartasNumero sobrantes
    let descartada = last (cartasOrdenadas) --SACAR UNA (EN LO POSIBLE LA MAS GRANDE)
    let mano' = init (cartasOrdenadas) ++ seguras ++ ss  -- y poner el resto en mano''
    let pilaDescartadas' = [descartada] ++ pilaDescartadas
    let computadora' = (Jugador nombre mano' seguras ss puntos)
    (jugador, computadora', mazo, pilaDescartadas')
    
    
descartarDeSemiSeguras ::  EstadoDeJuego -> Carta -> EstadoDeJuego
descartarDeSemiSeguras estado@(jugador, (Jugador nombre mano seguras ss puntos), mazo, pilaDescartadas) cLevantada = do    
    if length ss >= 1 then do
        let cartasOrdenadas = ordenarCartasNumero ss
        let descartada = last (cartasOrdenadas)
        let ssComb = [cartas | cartas <- combinaciones 2 ss, esSemiSegura cartas]
        let combEscalera = [cartas | cartas <- ssComb, esSemiSeguraPalo cartas, descartada `elem` cartas]
        let combNumeros = [cartas | cartas <- ssComb, esSemiSeguraNumero cartas,descartada `elem` cartas]
        let aMover = if length combEscalera >=1 then head combEscalera else head combNumeros
        let [aDejar] = [carta | carta <- aMover, not (carta == descartada)]
        let ss' = [carta | carta <- cartasOrdenadas, not (carta == descartada), not (carta == aDejar) ] 
        let pilaDescartadas' = [descartada] ++ pilaDescartadas
        let mano' = seguras ++ ss' ++ [aDejar]
        let computadora' = (Jugador nombre mano' seguras ss' puntos)
        (jugador, computadora', mazo, pilaDescartadas')
    else do
        let pilaDescartadas' = [cLevantada] ++ pilaDescartadas
        let mano' = [carta | carta <- mano, not (carta == cLevantada)]
        let computadora' = (Jugador nombre mano' seguras ss puntos)
        (jugador, computadora', mazo, pilaDescartadas')    
    
cartaDesconocidaComputadora :: EstadoDeJuego -> EstadoDeJuego
cartaDesconocidaComputadora estado@(jugador, (Jugador nombre mano seguras ss puntos), mazo, pilaDescartadas) = do
    let cLevantada = cartaDesconocida mazo
    let mano' = [cLevantada] ++ mano
    let mazo' = tail mazo
    let posibles = [cuatro | cuatro <- combinaciones 4 mano'\\(combinaciones 3 seguras), esBuena cuatro, cLevantada `elem` cuatro]
    if length posibles >=1
    then do
        let combElegida = if length [cartas | cartas <- posibles, cartasEnSecuencia cartas] >= 1 then last [cartas | cartas <- posibles, cartasEnSecuencia cartas] else last posibles
        let seguras' = removerDuplicados (seguras ++ combElegida)
        let ss' = [carta | carta <- ss, carta `notElem` seguras']
        --        let sobrantes = [carta | carta <- mano', carta `notElem` (seguras' `union` ss)] -- TODAS LAS QUE NO ESTAN EN SEGURAS Y SS
        let sobrantes = (mano' \\ seguras') \\ ss'
        if length sobrantes > 0
        then do
            descartarSobrante (jugador, (Jugador nombre mano' seguras' ss' puntos), mazo', pilaDescartadas) sobrantes
        else do
            descartarDeSemiSeguras (jugador, (Jugador nombre mano' seguras' ss' puntos), mazo', pilaDescartadas) cLevantada
    else do
        let combPosibles = [tres | tres <- combinaciones 3 (mano'\\seguras)\\(combinaciones 2 ss), esBuena tres &&  cLevantada `elem` tres]
        if length combPosibles >=1
        then do
            -- ACA NO ESTA SACANDO LA CARTA QUE SOBRA DE UN SS QUE PASA A SEGURA
            let combElegida = if length [cartas | cartas <- combPosibles, cartasEnSecuencia cartas] >= 1 then last [cartas | cartas <- combPosibles, cartasEnSecuencia cartas] else last combPosibles
            let seguras' = seguras ++ combElegida
            let ss' = [carta | carta <- ss, carta `notElem` seguras']
            let ssComb =  [carta | carta <- combinaciones 2 ss', esSemiSegura carta]
            let ss'' = removerDuplicados (concat ssComb)
            let sobrantes = [carta | carta <- mano', carta `notElem` (seguras' `union` ss'')] -- TODAS LAS QUE NO ESTAN EN SEGURAS Y SS
            if length sobrantes > 0
            then do
                descartarSobrante (jugador, (Jugador nombre mano' seguras' ss'' puntos), mazo', pilaDescartadas) sobrantes
            else do
                descartarDeSemiSeguras (jugador, (Jugador nombre mano' seguras' ss'' puntos), mazo', pilaDescartadas) cLevantada
        else do
            let resto = (mano' \\ seguras) \\ ss
            let combinaciones2 = [carta | carta <- combinaciones 2 resto, esSemiSegura carta]
            let combPalo = [carta | carta <- combinaciones2, esSemiSeguraPalo carta]
            let combNumero = [carta | carta <- combinaciones2, esSemiSeguraNumero carta]
            let combElegida = if (length combPalo) >=1 then head combPalo else if length (combNumero) >= 1 then head combNumero else []
            let resto' =  resto \\ combElegida
            let ss' = ss ++ combElegida
            if length resto' >= 1
            then do
                let restoOrdenado = ordenarCartasNumero resto'
                let descartada = last restoOrdenado
                let pilaDescartadas' = [descartada] ++ pilaDescartadas
                let mano'' = [carta | carta <- mano', not (carta == descartada)]
                (jugador, (Jugador nombre mano'' seguras ss' puntos), mazo', pilaDescartadas')
            else do
                let ss' = ss ++ combElegida
                descartarDeSemiSeguras (jugador, (Jugador nombre mano' seguras ss' puntos), mazo', pilaDescartadas) cLevantada
            
                        
    
configurarComputadora :: Jugador -> Jugador
configurarComputadora (Jugador nombre mano seguras ss puntos) = do
    let combinaciones4 = [carta | carta <- combinaciones 4 mano, esBuena carta]
    let todasCombinaciones4 = removerDuplicados (concat combinaciones4)
    let difCombinaciones4 = mano \\ todasCombinaciones4
    let sobrantes = if length difCombinaciones4 >= 1 then difCombinaciones4 else mano
    let combinaciones3 = [carta | carta <- combinaciones 3 sobrantes, esBuena carta]
    let todasCombinaciones3 = removerDuplicados (concat combinaciones3)
    let difCombinaciones3 = sobrantes \\ todasCombinaciones3
    let sobrantes' = if length difCombinaciones3 >= 1 then difCombinaciones3 else sobrantes
    let seguras' = todasCombinaciones4 ++ todasCombinaciones3
    -- DEBERIAMOS VER ESTO --> ME HIZO UN SSEGURAS DE 3
    let combinaciones2 = [carta | carta <- combinaciones 2 sobrantes', esSemiSegura carta]
    let ss' = removerDuplicados (concat combinaciones2)
    (Jugador nombre mano seguras' ss' puntos)


removerDuplicados :: Eq a => [a] -> [a]
removerDuplicados = foldl (\visto x -> if x `elem` visto
    then visto
    else visto ++ [x]) []
    
    
mostrarManoCompu :: Jugador -> String
mostrarManoCompu (Jugador _ mano _ _ _) = show mano

devolverComputadoraEstado :: EstadoDeJuego -> Jugador
devolverComputadoraEstado (_, computadora, _, _) = computadora


calcularPuntos :: Jugador -> Int
calcularPuntos (Jugador _ mano seguras _ puntos)
    -- IF DA MENOS 10 -> 0 porque es el que no corto
    | puedeGanarMenosDiez mano = 0
    | puedeGanarSobraUna mano = buscarCombinacionesRestoMasBajo mano
--    | tieneCinco mano = calcularRestoCombinaciones mano
    | otherwise = calcularRestoCombinaciones mano

     
--tieneCinco :: [Carta] -> [Carta]
--tieneCinco mano = do   
--    let combinacionesPosibles = [comb | comb <- combinaciones 5 mano, tieneNSuc comb 5]
--    length combinacionesPosibles >= 1

calcularRestoCombinaciones :: [Carta] -> Int
calcularRestoCombinaciones mano = do   
    let mano' = ordenarCartasNumero mano
    let combinacionesPosibles = [comb | comb <- combinaciones 5 mano', tieneNSuc comb 5]
    if length combinacionesPosibles >= 1 
    then do
        let combinacionMayor = (last combinacionesPosibles)
        sum (map obtenerValorNumerico (mano'\\combinacionMayor))
    else
        calcularDependiendoCombinacion mano 4

calcularDependiendoCombinacion :: [Carta] -> Int -> Int
calcularDependiendoCombinacion mano 2 = sum (map obtenerValorNumerico mano)
calcularDependiendoCombinacion mano nCartas = do
    -- Si n == 4 ELIJE 1 SOLA COMBINACION PORQUE SINO HUBIESE ENTRADO EN puedeGanarMenosDiez
    -- Si n == 3 ELIJE 1 SOLA COMBINACION PORQUE SINO HUBIESE ENTRADO EN puedeGanarSobraUna
    let combinacionesPosibles = [ (CombinacionPuntaje comb suma) | comb <- combinaciones nCartas mano, esBuena comb, let suma = sum (map obtenerValorNumerico comb) ]
    if length combinacionesPosibles >= 1 
    then do
        let combinacionesPosiblesOrdenadas = ordernarCombinacionPuntaje combinacionesPosibles
        let combinacionMayor = combinacion (last combinacionesPosiblesOrdenadas)
        sum (map obtenerValorNumerico (mano\\combinacionMayor))
    else
        calcularDependiendoCombinacion mano (nCartas-1)
        
buscarCombinacionesRestoMasBajo :: [Carta] -> Int
buscarCombinacionesRestoMasBajo mano =  do
                                        let buenasCombinaciones mano' = [ (tres, tres'') | tres <- combinaciones 3 mano',let tres' = mano' \\ tres, tres'' <- combinaciones 3 tres', esBuena tres, esBuena tres'']
                                        let restos mano' = concat [ (mano'\\tres)\\tres'' | tres <- combinaciones 3 mano',let tres' = mano' \\ tres, tres'' <- combinaciones 3 tres', esBuena tres, esBuena tres'']
                                        obtenerValorNumerico (head (ordenarCartasNumero (restos mano)))
--                                        
            
ordernarCombinacionPuntaje :: [CombinacionPuntaje] -> [CombinacionPuntaje]
ordernarCombinacionPuntaje = sortBy (comparing suma)

