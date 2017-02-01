import Chinchon
import System.Random

main :: IO ()
main = do

	estadoInicial <- setup
	jugar estadoInicial 1 0
	
setup :: IO (EstadoDeJuego)
setup = do
	-- shuffle cards
	mazo <- shuffleIO nuevoMazo
	-- creamos jugador
	putStrLn "Cual es tu nombre?"
	jugador <- nuevoJugador <$> getLine
	
	let computadora = nuevoJugador "Computadora"
	-- repartir 7 cartas a todos los jugadores
	let (mazo', jugador') = repartirNCartasAJugador 7 mazo jugador
	let jugador'' = (Jugador (nombre jugador') (ordenarTodasCartas (mano jugador')) [] [])
	let (mazo'', computadora') = repartirNCartasAJugador 7 mazo' computadora
	-- turn 1 card and make a discarded pile
	let (cartaDescartada, mazoAux) = repartir mazo''
	let pilaDescartada = [cartaDescartada]

	return (jugador'', computadora', mazoAux, pilaDescartada)
	
esPar :: Int -> Bool
esPar 0 = True
esPar n = n `rem` 2 == 0

jugar :: EstadoDeJuego -> Int -> Int-> IO ()
jugar estado@(jugador, computadora, _,_) turno corto =
	if corto == 1
		then if puedeGanar jugador
			then putStrLn $ "Ganaste! " ++ mostrarMano estado
		else do
			putStrLn $ "No podés cortar todavia! Intentá cuando tengas combinaciones.\n"
			jugar estado (turno) 0
	else if corto == 2 
		then putStrLn $ "Gano la computadora! " ++ mostrarMano estado
	else
		if not (esPar turno)
		then do
			estado' <- levantarCarta estado
			putStrLn "Que querés hacer ahora? \n  1) Tirar carta\n  2) Tirar y cortar)"
			respuesta <- readLn
			estado'' <- descartarCarta estado'
			if respuesta == 2
				then jugar estado'' (turno) 1
			else
				jugar estado'' (turno+1) 0
		else do
--			estado' <- jugadaComputadora estado
--if esChincho......
			if puedeGanar computadora
			then
				jugar estado (turno) 2
			else
				jugar estado (turno+1) 0


levantarCarta :: EstadoDeJuego -> IO EstadoDeJuego
levantarCarta estado = do
	putStrLn $ "\nTu mano: " ++ mostrarMano estado
	putStrLn $ "Que te gustaría hacer ahora?\n  1) tomar una carta desconocida\n  2) tomar la ultima carta descartada (" ++ (mostrarUltimaCartaDescartada estado) ++ ")"
	respuesta <- readLn
	return $ (if respuesta == 1 then tomarCartaDesconocida estado else tomarUltimaCartaDescartada estado)


descartarCarta :: EstadoDeJuego -> IO EstadoDeJuego
descartarCarta ((Jugador nombre mano s ss), computadora, mazoRestante, pilaDescartadas) = do
	putStrLn $ "\nTu mano: " ++ show mano
	putStrLn "Elegí una carta a descartar (1-8)"
	n <- readLn
	-- VER COMO HACER PARA QUE NO TOME VALORES MENORES a 0 y MAYORES a 8
	let (mano', pilaDescartadas') = descartar mano n pilaDescartadas
	return ((Jugador nombre mano' s ss), computadora, mazoRestante, pilaDescartadas')
