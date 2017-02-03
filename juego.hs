import Chinchon
import System.Random
import System.IO
import Control.Monad 

puntajeMaximo = 100

main :: IO ()
main = do
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
jugar estado@(jugador, computadora, mazo, pilaDescartadas) turno corto  =
    if length mazo == 1
    then do    
        let pilaDescartadas' = head pilaDescartadas
        mazo' <- shuffleIO (tail pilaDescartadas)
        jugar (jugador, computadora, mazo', [pilaDescartadas']) turno corto
    else
        if (puntos jugador > puntajeMaximo) && (puntos computadora < puntajeMaximo)
        then do
            putStrLn $ "Perdiste! La computadora ganó el juego! \nTus puntos: " ++ show (puntos jugador) ++ "\nPuntos de la computadora: " ++ show (puntos computadora)
        else if (puntos jugador < puntajeMaximo) && (puntos computadora > puntajeMaximo)
        then putStrLn $ "Ganaste! La computadora perdió el juego! \nTus puntos: " ++ show (puntos jugador) ++ "\nPuntos de la computadora: " ++ show (puntos computadora)
        else if (puntos jugador > puntajeMaximo) && (puntos computadora > puntajeMaximo)
        then if (puntos jugador > puntos computadora)
            then putStrLn $ "Perdiste! La computadora ganó el juego! \nTus puntos: " ++ show (puntos jugador) ++ "\nPuntos de la computadora: " ++ show (puntos computadora)
            else putStrLn $ "Ganaste! La computadora perdió el juego! \nTus puntos: " ++ show (puntos jugador) ++ "\nPuntos de la computadora: " ++ show (puntos computadora)
        else 
            if corto == 1
                then 
                if esChinchon (mano jugador)
                    then putStrLn $ "Ganaste! La computadora perdió el juego! \nTus puntos: " ++ show (puntos jugador) ++ "\nPuntos de la computadora: " ++ show (puntos computadora + puntajeMaximo)
                else
                    if puedeGanarMenosDiez (mano jugador)
                        then do
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
--                            (jugador', computadora', mazo, pilaDescartadas) turno 0
                    else 
                        if puedeGanarSobraUna (mano jugador) 
                            then do 
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
                        else do
                        putStrLn $ "No podés cortar todavia! Intentá cuando tengas combinaciones.\n"
                        jugar estado (turno) 0
                
            else if corto == 2 
                then 
                if esChinchon (mano computadora)
                    then putStrLn $ "Perdiste! La computadora ganó el juego! \nTus puntos: " ++ show (puntos jugador + puntajeMaximo) ++ "\nPuntos de la computadora: " ++ show (puntos computadora)
                else
                    if puedeGanarMenosDiez (mano computadora)
                        then do
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
                    else 
                        if puedeGanarSobraUnaComputadora (mano computadora)
                            then do
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
                        else jugar estado (turno) 0
                
            else do
                if not (esPar turno)
                then do
                    putStrLn $ "\nEstado Compu Despues:" ++ show computadora
                    putStrLn $ "\nDescartadas por Compu:" ++ show pilaDescartadas
                    estado' <- levantarCarta estado
                    putStrLn $ mostrarMano estado'
                    putStrLn "Que querés hacer ahora? \n  1) Tirar carta\n  2) Tirar y cortar"
                    respuesta <- readLn
                    estado'' <- descartarCarta estado'
                    putStrLn $ mostrarMano estado''
                    if respuesta == 2
                        then jugar estado'' (turno) 1
                    else
                        jugar estado'' (turno+1) 0
                else do
--                    putStrLn $ "\nDescartadas por user:" ++ show pilaDescartadas
--                    putStrLn $ "Primera mazo: " ++ show (head mazo)
--                    putStrLn $ "\nEstado Compu:" ++ show computadora
                    putStrLn $ "Mano Compu Antes: " ++ mostrarManoCompu computadora
                    let estado' = jugadaComputadora estado
                    let computadoraDespues = devolverComputadoraEstado estado'
                    if puedeGanarComputadora computadoraDespues
                    then  jugar estado' (turno) 2
                    else jugar estado' (turno+1) 0


levantarCarta :: EstadoDeJuego -> IO EstadoDeJuego
levantarCarta estado = do
    putStrLn $ "\nTu mano: " ++ mostrarMano estado
    putStrLn $ "Que te gustaría hacer ahora?\n  1) tomar una carta desconocida\n  2) tomar la ultima carta descartada (" ++ (mostrarUltimaCartaDescartada estado) ++ ")"
    respuesta <- readLn
    return $ (if respuesta == 1 then tomarCartaDesconocida estado else tomarUltimaCartaDescartada estado)


descartarCarta :: EstadoDeJuego -> IO EstadoDeJuego
descartarCarta ((Jugador nombre mano s ss puntos), computadora, mazoRestante, pilaDescartadas) = do
    putStrLn $ "\nTu mano: " ++ show mano
    putStrLn "Elegí una carta a descartar (1-8)"
    n <- readLn
    -- VER COMO HACER PARA QUE NO TOME VALORES MENORES a 1 y MAYORES a 8
    let (mano', pilaDescartadas') = descartar mano n pilaDescartadas
    return ((Jugador nombre mano' s ss puntos), computadora, mazoRestante, pilaDescartadas')


mostrarPuntajes :: Int -> Int -> IO ()
mostrarPuntajes pJugador pComputadora = do
    putStrLn $ "\nPuntajes:"
    putStrLn $ "-----------------------------"
    putStrLn $ "Tu Puntaje: " ++ show pJugador
    putStrLn $ "\nPuntaje de la computadora: " ++ show pComputadora
    putStrLn $ "=============================\n"