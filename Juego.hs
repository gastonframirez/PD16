import Chinchon
import System.Random
import System.IO
import Control.Monad 

puntajeMaximo = 100

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetBuffering stdin LineBuffering
    estadoInicial <- configurar
    jugar estadoInicial 1 0

configurar :: IO (EstadoDeJuego)
configurar = do
    -- mezclar cartas
    mazo <- shuffleIO nuevoMazo
    -- crear jugador
    putStrLn "Cual es tu nombre?"
    jugador <- nuevoJugador <$> getLine
    
    let computadora = nuevoJugador "Computadora"
    -- repartir 7 cartas a todos los jugadores
    let (mazo', jugador') = repartirNCartasAJugador 7 mazo jugador
    let jugador'' = (Jugador (nombre jugador') (ordenarTodasCartas (mano jugador')) [] [] 0)
    let (mazo'', computadora') = repartirNCartasAJugador 7 mazo' computadora
    let computadora''  = configurarComputadora computadora'
    -- dar vuelta 1 carta y poner en pila de descartadas
    let (cartaDescartada, mazoAux) = repartir mazo''
    let pilaDescartada = [cartaDescartada]
    mostrarPuntajes 0 0
    return (jugador'', computadora'', mazoAux, pilaDescartada)


reConfigurar :: Jugador -> Jugador -> IO (EstadoDeJuego)
reConfigurar (Jugador nombreJugador _ _ _ puntosJugador) computadoraEst@(Jugador nombreComputadora manoComputadora _ _ puntosComputadora) = do
    -- mezclar cartas
    mazo <- shuffleIO nuevoMazo
    putStrLn $ "Mano de la computadora " ++ mostrarManoCompu computadoraEst    
    let jugador = (Jugador nombreJugador [] [] [] puntosJugador)
    let computadora = (Jugador nombreComputadora [] [] [] puntosComputadora)
    -- repartir 7 cartas a todos los jugadores
    let (mazo', jugador') = repartirNCartasAJugador 7 mazo jugador
    let jugador'' = (Jugador (nombre jugador') (ordenarTodasCartas (mano jugador')) [] [] puntosJugador)
    let (mazo'', computadora') = repartirNCartasAJugador 7 mazo' computadora
    let computadora''  = configurarComputadora computadora'
    -- dar vuelta 1 carta y poner en pila de descartadas
    let (cartaDescartada, mazoAux) = repartir mazo''
    let pilaDescartada = [cartaDescartada]
    mostrarPuntajes puntosJugador puntosComputadora
    return (jugador'', computadora'', mazoAux, pilaDescartada)



esPar :: Int -> Bool
esPar 0 = True
esPar n = n `rem` 2 == 0

jugar :: EstadoDeJuego -> Int -> Int -> IO ()
jugar estado@(jugador, computadora, mazo, pilaDescartadas) turno corto 
    | length mazo == 0 = do
        let pilaDescartadas' = head pilaDescartadas
        mazo' <- shuffleIO (tail pilaDescartadas)
        jugar (jugador, computadora,  mazo', [pilaDescartadas']) turno corto
    | corto == 1 && noPuedeCortar (mano jugador) = do
        putStrLn $ "No podés cortar todavia! Intentá cuando tengas combinaciones.\n"
        jugar estado (turno+1) 0
    | corto == 2 && noPuedeCortar (mano computadora) = jugar estado (turno+1) 0
    | (puntos jugador > puntajeMaximo) && (puntos computadora < puntajeMaximo) = putStrLn $ "Perdiste! La computadora ganó el juego con Chinchón! \nTus puntos: " ++ show (puntos jugador) ++ "\nPuntos de la computadora: " ++ show (puntos computadora)
    | (puntos jugador < puntajeMaximo) && (puntos computadora > puntajeMaximo) = putStrLn $ "Ganaste con Chinchón! La computadora perdió el juego! \nTus puntos: " ++ show (puntos jugador) ++ "\nPuntos de la computadora: " ++ show (puntos computadora)
    | (puntos jugador > puntajeMaximo) && (puntos computadora > puntajeMaximo) && (puntos jugador > puntos computadora) = putStrLn $ "Perdiste! La computadora ganó el juego! \nTus puntos: " ++ show (puntos jugador) ++ "\nPuntos de la computadora: " ++ show (puntos computadora)
    | (puntos jugador > puntajeMaximo) && (puntos computadora > puntajeMaximo) && not (puntos jugador > puntos computadora) = putStrLn $ "Ganaste! La computadora perdió el juego! \nTus puntos: " ++ show (puntos jugador) ++ "\nPuntos de la computadora: " ++ show (puntos computadora)
    | corto == 1 &&  esChinchon (mano jugador) = putStrLn $ "Ganaste! La computadora perdió el juego! \nTus puntos: " ++ show (puntos jugador) ++ "\nPuntos de la computadora: " ++ show (puntos computadora + puntajeMaximo)
    | corto == 1 && puedeGanarMenosDiez (mano jugador) = do
        putStrLn $ "Ganaste la ronda con menos 10! " ++ mostrarMano estado
        let puntosComputadora = calcularPuntos computadora
        let puntosTotalComputadora = puntos computadora + puntosComputadora
        let puntosTotalJugador = puntos jugador - 10
        let puntosJugador = calcularPuntos jugador
        putStrLn $ "\nLa computadora suma: " ++ show puntosComputadora
        putStrLn $ "\n\n\n"
        let computadora' = (Jugador (nombre computadora) (mano computadora) (seguras computadora) (semiSeguras computadora) puntosTotalComputadora)
        let jugador' = (Jugador (nombre jugador) (mano jugador) (seguras jugador) (semiSeguras jugador) puntosTotalJugador)
        estado' <- reConfigurar jugador' computadora'
        jugar estado' turno 0
    | corto == 1 && puedeGanarSobraUna (mano jugador) = do 
        let puntosComputadora = calcularPuntos computadora
        let puntosTotalComputadora = puntos computadora + puntosComputadora
        let puntosJugador = calcularPuntos jugador
        let puntosTotalJugador = puntos jugador + puntosJugador

        putStrLn $ "Ganaste la ronda! Sumas:  " ++ show puntosJugador 
        putStrLn $ "\nLa computadora suma: " ++ show puntosComputadora
        putStrLn $ "\n\n\n"
        let computadora' = (Jugador (nombre computadora) (mano computadora) (seguras computadora) (semiSeguras computadora) puntosTotalComputadora)
        let jugador' = (Jugador (nombre jugador) (mano jugador) (seguras jugador) (semiSeguras jugador) puntosTotalJugador)
        estado' <- reConfigurar jugador' computadora'
        jugar estado' turno 0   

    | corto == 2 &&  esChinchon (mano computadora) = putStrLn $ "Perdiste! La computadora ganó el juego! \nTus puntos: " ++ show (puntos jugador + puntajeMaximo) ++ "\nPuntos de la computadora: " ++ show (puntos computadora)
    | corto == 2 && puedeGanarMenosDiez (mano computadora) = do
        putStrLn $ "Computadora gano la ronda, con menos 10! "
        let puntosComputadora = calcularPuntos computadora
        let puntosTotalComputadora = puntos computadora - 10
        let puntosJugador = calcularPuntos jugador
        let puntosTotalJugador = puntos jugador + puntosJugador
        putStrLn $ "\nVos sumás: " ++ show puntosJugador
        putStrLn $ "\n\n\n"
        let computadora' = (Jugador (nombre computadora) (mano computadora) (seguras computadora) (semiSeguras computadora) puntosTotalComputadora)
        let jugador' = (Jugador (nombre jugador) (mano jugador) (seguras jugador) (semiSeguras jugador) puntosTotalJugador)

        estado' <- reConfigurar jugador' computadora'
        jugar estado' turno 0
    | corto == 2 && puedeGanarSobraUna (mano computadora) = do
        let puntosComputadora = calcularPuntos computadora
        let puntosTotalComputadora = puntos computadora + puntosComputadora
        let puntosJugador = calcularPuntos jugador
        let puntosTotalJugador = puntos jugador + puntosJugador
        putStrLn $ "Computadora gano la ronda, suma: " ++ show puntosComputadora
        putStrLn $ "\nVos sumás: " ++ show puntosJugador
        let computadora' = (Jugador (nombre computadora) (mano computadora) (seguras computadora) (semiSeguras computadora) puntosTotalComputadora)
        let jugador' = (Jugador (nombre jugador) (mano jugador) (seguras jugador) (semiSeguras jugador) puntosTotalJugador)
        estado' <- reConfigurar jugador' computadora'
        jugar estado' turno 0 
    
    | not (esPar turno) = do
        estado' <- levantarCarta estado
        putStrLn "Que querés hacer ahora? \n  1) Tirar carta\n  2) Tirar y cortar"
        respuesta <- readLn
        estado'' <- descartarCarta estado'
        if respuesta == 2
            then jugar estado'' (turno) 1
        else
            jugar estado'' (turno+1) 0
      
    | otherwise = do
        let estado' = jugadaComputadora estado
        let computadoraDespues = devolverComputadoraEstado estado'
        if puedeGanarComputadora computadoraDespues
        then  jugar estado' (turno) 2
        else jugar estado' (turno+1) 0


levantarCarta :: EstadoDeJuego -> IO EstadoDeJuego
levantarCarta estado = do
    putStrLn $ "\n----------------------------------------------"
    putStrLn $ "\nTu mano: " ++ mostrarMano estado
    putStrLn $ "Que te gustaría hacer ahora?\n  1) tomar una carta desconocida\n  2) tomar la ultima carta descartada (" ++ (mostrarUltimaCartaDescartada estado) ++ ")"
    respuesta <- readLn
    return $ (if respuesta == 1 then tomarCartaDesconocida estado else tomarUltimaCartaDescartada estado)


descartarCarta :: EstadoDeJuego -> IO EstadoDeJuego
descartarCarta ((Jugador nombre mano s ss puntos), computadora, mazoRestante, pilaDescartadas) = do
    putStrLn $ "\nTu mano: " ++ show mano
    putStrLn "Elegí una carta a descartar (1-8)"
    n <- readLn
    let (mano', pilaDescartadas') = descartar mano n pilaDescartadas
    return ((Jugador nombre mano' s ss puntos), computadora, mazoRestante, pilaDescartadas')


mostrarPuntajes :: Int -> Int -> IO ()
mostrarPuntajes pJugador pComputadora = do
    putStrLn $ "\nPuntajes:"
    putStrLn $ "------------------------------------------------------------------------------------------"
    putStrLn $ "Tu Puntaje: " ++ show pJugador
    putStrLn $ "\nPuntaje de la computadora: " ++ show pComputadora
    putStrLn $ "==========================================================================================\n"