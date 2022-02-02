-- ---------------------------------------------------------------------
-- Imports
-- ---------------------------------------------------------------------
import System.Exit -- para poder terminar el programa
import Control.Concurrent -- para poder hacer los delays
import System.Console.ANSI -- para poder limpiar la pantalla
import System.IO -- para desactivar el buffering en la salida
import Text.CSV -- para leer las partidas guardadas .csv
import Functions.ColaConListas -- para hacer uso de colas
-- ---------------------------------------------------------------------
-- Módulos propios
-- ---------------------------------------------------------------------
-- Encargado de las matrices del ejército de soldados:
import Functions.Matriz 

-- Encargado de modificar los valores de las partidas guardadas:
import Functions.ModificaDatos 

-- Encargado de los números aleatorios que se utilizan para la cola de 
-- la tienda(la cola que te encuentras es aleatoria), las probabilidades
-- de cada mineral en la mina y la probabilidad de cazar un conejo en la
-- montaña: 
import Functions.RandomNumber 

-- Encargado de los tipos algebráicos de los objetos y de las personas 
-- que se pueden consultar en el ayuntamiento del pueblo:
import Functions.TipoAlgebraico

import Functions.Guardia -- Encargado de las funciones del guardia
import Functions.Bufon -- Encargado de las funciones del bufón
import Functions.Tienda -- Encargado de las funciones de la tienda
import Functions.Frutero -- Encargado de las funciones del frutero
import Functions.Gigante -- Encargado de las funciones del gigante

-- ---------------------------------------------------------------------
-- Variables Globales 
-- ---------------------------------------------------------------------

pico :: Bool
pico = True

tirachinasBool :: Bool
tirachinasBool = False

-- -----------------------------------------------------
-- Funciones Principales:
--------------------------------------------------------
-- Inventario
--------------------------------------------------------
inventario :: IO ()
inventario = do 
    clearScreen
    readsg <- readFile "savegames/temp.txt"
    let savegame = parseCSV "savegames/temp.txt" readsg
        datos = case savegame of
          (Right lineas) -> lineas
          _ -> []
    
    let nombre = head (head datos)
    let nivel = head (head (drop 1 datos))
    let dinero = head (head (drop 2 datos))
    let cobre = head (head (drop 4 datos))
    let plata = head (head (drop 5 datos))
    let oro = head (head (drop 6 datos))
    let carne = head (head (drop 7 datos))
    let cadena1 = "Doblones: "++dinero
    let cadena2 = "Cobre: "++cobre
    let cadena3 = "Plata: "++plata
    let cadena4 = "Oro: "++oro    
    let cadena5 = "Carne de conejo: "++carne
    putStrLn "--------------------------------------------------------------------"
    putStrLn "                        - INVENTARIO -                              "
    putStrLn "--------------------------------------------------------------------"
    print cadena1
    print cadena2
    print cadena3
    print cadena4
    print cadena5
    putStrLn "--------------------------------------------------------------------"
    putStrLn "    Escribe cualquier tecla para volver al menú principal"
    putStrLn "--------------------------------------------------------------------"
    opc <- getLine
    menuInGame
    
--------------------------------------------------------
-- Cima de la Montaña
--------------------------------------------------------
cima :: IO()
cima = do 
    clearScreen
    putStrLn "--------------------------------------------------------------------"
    putStrLn " Llegas a la cima de la montaña y observas un espartano sentado en"
    putStrLn " una roca, parece preocupado. Decides acercarte a él."
    putStrLn "--------------------------------------------------------------------"
    threadDelay 5000000
    clearScreen
    putStrLn "-------------------------------------------------------------------------"
    putStrLn "- Sargento: Eh amigo, ¿Puedes ayudarme? Tengo a mi ejercito ahí abajo"
    putStrLn "            en ese prado que se observa desde aquí y necesito contarlos"
    putStrLn "            para asegurarme de que no se ha escaqueado ninguno, pero tengo"
    putStrLn "            la vista algo cansada y no veo desde aquí..."
    putStrLn "-------------------------------------------------------------------------"
    putStrLn "            1. ¡Claro no te preocupes!"
    putStrLn "            2. Soy incapaz de verlos desde tan lejos, lo siento."
    putStrLn "-------------------------------------------------------------------------"
    opc <- getLine
    if head opc=='1' then do
        clearScreen
        putStrLn "---------------------------------------------------------------------------"
        putStrLn " Observas desde arriba como el pelotón está en perfecta formación:"
        threadDelay 2000000
        print (ejercito)
        threadDelay 2000000
        putStrLn " Lo imaginas como una matriz en tu cabeza y multiplicas filas por columnas:"
        threadDelay 2000000
        print (calculaEjercito ejercito)
        putStrLn "---------------------------------------------------------------------------"
        threadDelay 4000000
        clearScreen
        putStrLn "-------------------------------------------------------------------------"
        putStrLn "- Sargento: ¿25? ¡Sí, entonces están todos! ¡Muchísimas gracias joven!"
        putStrLn "            Toma esto por las molestias:"
        putStrLn "---------------------------------------------------------------------------"
        threadDelay 4000000
        putStrLn "                      - Obtienes 50 doblones -"
        sumaDinero 50
        threadDelay 3000000
        menuInGame
    else if head opc=='2' then do 
        clearScreen
        putStrLn "--------------------------------------------------------------"
        putStrLn "- Sargento: ¿Y entonces por qué sigues aquí? ¡Fuera!"
        putStrLn "--------------------------------------------------------------"
        threadDelay 3000000
        menuInGame
    else do menuInGame
--------------------------------------------------------
-- Caza
--------------------------------------------------------
cazar :: IO()
cazar = do
    clearScreen
    putStrLn "-----------------------------------------------------------------------"
    putStrLn "Te adentras entre los arbustos y te dispones a cazar algún conejo..."
    putStrLn "-----------------------------------------------------------------------"
    threadDelay 2000000

    if (tirachinasBool) then do
        aleatorio<-numeroAleatorio
        clearScreen
        putStrLn "----------------------------------------------------------------------------------------------"
        putStrLn "Ves un conejo corretear en frente, coges una piedra del camino y la colocas en tu tirachinas"
        putStrLn "----------------------------------------------------------------------------------------------"
        threadDelay 3000000
        putStr "Apuntando"
        threadDelay 1000000
        putStr "."
        threadDelay 1000000
        putStr "."
        threadDelay 1000000
        putStr ". "
        if ((aleatorio>=0) && (aleatorio<=7)) then do
            putStrLn "¡Justo en el blanco! Hoy tienes cena"
            threadDelay 2000000
            putStrLn " - Obtienes carne de conejo -"
            sumaCarne 1
            threadDelay 2000000
            menuInGame
        else do 
            putStrLn "Vaya, has fallado el tiro, el conejo se ha escapado..."
            threadDelay 2000000
            menuInGame
    else do
        aleatorio<-numeroAleatorio
        clearScreen
        putStrLn "----------------------------------------------------------------------------------------------"
        putStrLn "Ves un conejo corretear en frente, coges una piedra del camino e intentas acertar con ella..."
        putStrLn "----------------------------------------------------------------------------------------------"
        threadDelay 3000000
        putStr "Apuntando"
        threadDelay 1000000
        putStr "."
        threadDelay 1000000
        putStr "."
        threadDelay 1000000
        putStr ". "
        if ((aleatorio>=0) && (aleatorio<=2)) then do
            putStrLn "¡Justo en el blanco! Hoy tienes cena"
            threadDelay 2000000
            putStrLn " - Obtienes carne de conejo -"
            sumaCarne 1
            threadDelay 2000000
            menuInGame
        else do 
            putStrLn "Vaya, has fallado el tiro, quizás esta no sea la mejor forma de cazar..."
            threadDelay 4000000
            menuInGame
--------------------------------------------------------
-- Montaña
--------------------------------------------------------

montana :: IO()
montana = do
    clearScreen
    putStrLn "-----------------------------------------------------------------------"
    putStrLn "Tras una larga caminata llegas a la ladera principal de la montaña."
    putStrLn "Puedes observar conejos corretear entre los arbustos de vez en cuando."
    putStrLn "-----------------------------------------------------------------------"
    putStrLn "            1. Intentar cazar un conejo"
    putStrLn "            2. Seguir hacia la cima de la montaña"
    putStrLn "-----------------------------------------------------------------------"
    opc <- getLine
    if head opc=='1' then do
        cazar
    else if head opc=='2' then do 
        cima
    else do 
        menuInGame

--------------------------------------------------------
-- Ayuntamiento 
--------------------------------------------------------
ayuntamiento :: IO()
ayuntamiento = do
    clearScreen
    putStrLn "--------------------------------------------------------------------"
    putStrLn "- Llegas a una sala enorme con un pilar sobre el que descansa un    "
    putStrLn "  libro gordísimo en el que pone: CENSO"
    putStrLn "--------------------------------------------------------------------"
    putStrLn "                   1. Buscar a alguien"
    putStrLn "                   2. Irse"
    putStrLn "--------------------------------------------------------------------"
    opc <- getLine
    if head opc=='1' then do
        clearScreen
        putStrLn "--------------------------------------------------------------------"
        putStrLn "                     ¿A quién buscas?"
        putStrLn "--------------------------------------------------------------------"
        putStrLn "            1. Buscar información sobre el guardia"
        putStrLn "            2. Buscar información sobre el frutero"
        putStrLn "            3. Buscar información sobre el gigante"
        putStrLn "            4. Buscar información sobre el tendero"
        putStrLn "            5. Buscar información sobre el bufón"
        putStrLn "--------------------------------------------------------------------"
        n <- getLine
        if (n=="1" || n=="2" || n=="3" || n=="4" || n=="5") then do
            clearScreen
            putStrLn "--------------------------------------------------------------------"
            putStrLn "                           FICHA :"
            putStrLn "--------------------------------------------------------------------"
            putStr "Nombre: "
            print (nombrePersona (personaElegida n))
            putStr "Edad(años): "
            print ((edad (personaElegida n)))
            putStr "Altura(cm): "
            print (altura (personaElegida n))
            putStr "Peso(Kg): "
            print (peso (personaElegida n))
            putStrLn "--------------------------------------------------------------------"
            threadDelay 6000000
            ayuntamiento
        else do
            ayuntamiento
    else if head opc=='2' then do
        pueblo
    else do 
        pueblo

--------------------------------------------------------
-- Guardia
--------------------------------------------------------
guardia :: IO()
guardia = do 
    clearScreen
    putStrLn "--------------------------------------------------------------------"
    putStrLn "- Guardia: Aquí Guybrush para servirle, guardia del reino de Eqens."
    putStrLn "           ¿En qué puedo ayudarle?"
    putStrLn "--------------------------------------------------------------------"
    putStrLn "            1. Hablame sobre este pueblo"
    putStrLn "            2. Hablame sobre la tienda"
    putStrLn "            3. Hablame sobre la montaña"
    putStrLn "            4. Hablame sobre la mina"
    putStrLn "            5. Creo que ya me iba..."
    putStrLn "------------------------------------------------------------------"
    opc <- getLine
    if head opc=='1' then do
        clearScreen
        putStrLn "--------------------------------------------------------------------"
        putStrLn ("- Guardia: "++dialogoGuardia 1)
        putStrLn "--------------------------------------------------------------------"
        threadDelay 6000000
        guardia
    else if head opc=='2' then do 
        clearScreen
        putStrLn "--------------------------------------------------------------------"
        putStrLn ("- Guardia: "++dialogoGuardia 2)
        putStrLn "--------------------------------------------------------------------"
        threadDelay 6000000
        guardia
    else if head opc=='3' then do 
        clearScreen
        putStrLn "--------------------------------------------------------------------"
        putStrLn ("- Guardia: "++dialogoGuardia 3)
        putStrLn "--------------------------------------------------------------------"
        threadDelay 6500000
        guardia
    else if head opc=='4' then do 
        clearScreen
        putStrLn "--------------------------------------------------------------------"
        putStrLn ("- Guardia: "++dialogoGuardia 4)
        putStrLn "--------------------------------------------------------------------"
        threadDelay 6000000
        guardia
    else if head opc=='5' then do
        pueblo
    else do
        clearScreen
        putStrLn "--------------------------------------------------------------------"
        putStrLn "- Guardia: No hablo arameo eh"
        putStrLn "--------------------------------------------------------------------"
        threadDelay 4000000
        pueblo

--------------------------------------------------------
-- Frutero 
--------------------------------------------------------
frutero :: IO()
frutero = do 
    clearScreen
    putStrLn "------------------------------------------------------------------"
    putStrLn "- Frutero: ¡Jamás encontraré mis pociones entre tanta fruta!"
    putStrLn "           ¿¿Alguien puede ayudarme??"
    putStrLn "------------------------------------------------------------------"
    putStrLn "            1. ¿Qué necesitas?"
    putStrLn "            2. Tengo prisa, lo siento"
    putStrLn "------------------------------------------------------------------"
    opc <- getLine
    if head opc=='1' then do
        clearScreen
        putStrLn "------------------------------------------------------------------"
        putStrLn "- Frutero: Tengo estas dos cajas llenas de todos los tipos de"
        putStrLn "  fruta que tengo en mi tienda, y en esta primera caja se me han"
        putStrLn "  caído dos de las pociones que acabo de comprar, pero entre tanta"
        putStrLn "  fruta soy incapaz de encontrarlas... ¿Crees poder ayudarme?"
        putStrLn "------------------------------------------------------------------"
        threadDelay 7000000
        putStrLn "            1. ¡Claro!"
        putStrLn "            2. Lo siento, me he mareado con tanta fruta..."
        putStrLn "------------------------------------------------------------------"
        opc2 <- getLine
        if head opc2 =='1' then do 
            clearScreen
            putStrLn "------------------------------------------------------------------------------"
            putStrLn "- Frutero: ¡Genial! Estos son los dos cajones de fruta:"
            putStrLn ""
            putStr "Cajón 1: " 
            print (cajon1)
            putStrLn ""
            putStr "Cajón 2: " 
            print (cajon2)
            putStrLn ""
            putStrLn "------------------------------------------------------------------------------"
            threadDelay 10000000
            putStrLn "- Utilizas tus conocimientos de PD para hacer mentalmente un filter que"
            putStrLn "  te indique qué elementos están en el cajón 1 pero no en el cajón 2 - :"
            threadDelay 7000000
            print (diferentes cajon1 cajon2)
            threadDelay 5000000
            clearScreen
            putStrLn "------------------------------------------------------------------"
            putStrLn "- Frutero: ¡Muchísimas gracias! ¡Nunca las habría encontrado sin"
            putStrLn "            tu ayuda!"
            putStrLn "------------------------------------------------------------------"
            threadDelay 4000000
            clearScreen
            putStrLn "------------------------------------------------------------------"
            putStrLn "- Frutero: Toma esto por las molestias"
            putStrLn "------------------------------------------------------------------"
            threadDelay 2000000
            putStrLn "                - Obtienes 20 doblones -"
            sumaDinero 20
            threadDelay 2000000
            pueblo
        else if head opc2=='2' then do 
            pueblo
        else do 
        clearScreen
        putStrLn "-------------------------------------------------------------------------------"
        putStrLn "- Frutero: Si no vas a ayudarme, vete."
        putStrLn "-------------------------------------------------------------------------------"
        threadDelay 2000000
        pueblo
    else if head opc=='2' then do
        clearScreen
        putStrLn "-------------------------------------------------------------------------------"
        putStrLn "- Frutero: ¡Piérdete!"
        putStrLn "-------------------------------------------------------------------------------"
        threadDelay 2000000
        pueblo
    else do 
        clearScreen
        putStrLn "-------------------------------------------------------------------------------"
        putStrLn "- Frutero: Si no vas a ayudarme, vete."
        putStrLn "-------------------------------------------------------------------------------"
        threadDelay 2000000
        pueblo

--------------------------------------------------------
-- Gigante 
--------------------------------------------------------
gigante :: IO()
gigante = do 
    clearScreen
    putStrLn "------------------------------------------------------------------"
    putStrLn "- Gigante: JAJAJA ¿QUÉ HACES AHÍ ABAJO? ¿CUÁNTO MIDES? ¿5cm?"
    putStrLn "------------------------------------------------------------------"
    putStrLn "                        (Indicar en cm)"
    putStr "Mido "
    a <- getLine
    clearScreen
    putStrLn "-------------------------------------------------------------------------------"
    putStrLn ("- Gigante: ¿"++a++"cm?")
    putStrLn "-------------------------------------------------------------------------------"
    threadDelay 2000000
    clearScreen
    putStrLn "-------------------------------------------------------------------------------"
    putStrLn ("- Gigante: a ojo podría decir que mido como "++show(alturaConversion a 0)++" personas como tú y un poco más")
    putStrLn "------------------------------------------------------------------------------"
    threadDelay 6000000
    pueblo

--------------------------------------------------------
-- Chistes y adivinanzas del bufón
--------------------------------------------------------
adivinanza :: IO()
adivinanza = do 
    clearScreen
    putStrLn "------------------------------------------------------------------"
    putStrLn "- Bufón: ¿Qué significan las siglas PD?"
    putStrLn "------------------------------------------------------------------"
    putStrLn "            1. Programación didáctica"
    putStrLn "            2. Programación declarativa"
    putStrLn "            3. Programación deductiva"
    putStrLn "------------------------------------------------------------------"
    opc <- getLine
    if head opc=='1' then do
        clearScreen
        putStrLn "------------------------------------------------------------------"
        putStrLn ("- Bufón: "++(adivinanzaF 1))
        putStrLn "------------------------------------------------------------------"
        threadDelay 3000000
        bufon
    else if head opc=='2' then do
        clearScreen
        putStrLn "-------------------------------------------------------------------------------"
        putStrLn ("- Bufón: "++(adivinanzaF 2))
        putStrLn "-------------------------------------------------------------------------------"
        threadDelay 3000000
        bufon
    else if head opc=='3' then do 
        clearScreen
        putStrLn "-------------------------------------------------------------------------------"
        putStrLn ("- Bufón: "++(adivinanzaF 3))
        putStrLn "-------------------------------------------------------------------------------"
        threadDelay 3000000
        bufon
    else do 
        adivinanza
chiste :: IO()
chiste = do
    clearScreen
    putStrLn "------------------------------------------------------------------"
    putStrLn "- Bufón: ¿Te apetece un chiste corto o largo?"
    putStrLn "------------------------------------------------------------------"
    putStrLn "                     1. Corto, que no tengo mucho tiempo..."
    putStrLn "                     2. ¡Largo!"
    putStrLn "------------------------------------------------------------------"
    opc1 <- getLine
    if head opc1=='1' then do
        clearScreen
        putStrLn "------------------------------------------------------------------"
        putStrLn ("- Bufón: "++(head (cuentaChiste 1)))
        putStrLn "------------------------------------------------------------------"
        threadDelay 3000000
        bufon
    else if head opc1=='2' then do
        clearScreen
        putStrLn "-------------------------------------------------------------------------------"
        putStrLn ("- Bufón: "++(head (cuentaChiste 2)))
        putStrLn "-------------------------------------------------------------------------------"
        threadDelay 3000000
        bufon
    else do 
        putStrLn "- Bufón: No te he entendido"
        threadDelay 1000000
        bufon
--------------------------------------------------------
-- Menú del Bufón
--------------------------------------------------------
bufon :: IO()
bufon = do
    clearScreen
    putStrLn "------------------------------------------------------------------"
    putStrLn "- Bufón: ¿Qué quieres joven?"
    putStrLn "------------------------------------------------------------------"
    putStrLn "                     1. ¡Cuéntame un chiste!"
    putStrLn "                     2. Dime una adivinanza"
    putStrLn "                     3. Ehh nada yo ya me iba"
    putStrLn "------------------------------------------------------------------"
    opc1 <- getLine
    if head opc1=='1' then do
        chiste
    else if head opc1=='2' then do
        adivinanza
    else if head opc1=='3' then do 
        pueblo
    else do 
        putStrLn "No te he entendido"
        threadDelay 1000000
        pueblo


--------------------------------------------------------
-- Tienda
--------------------------------------------------------

cola :: IO()
cola = do
    clearScreen
    putStrLn "------------------------------------------------------------------"
    putStrLn " Vaya, parece que hay cola para entrar a la tienda... tendrás que"
    putStrLn " esperar tu turno."
    putStrLn "------------------------------------------------------------------"
    threadDelay 5000000
    clearScreen
    putStrLn "------------------------------------------------------------------"
    putStrLn "                          Cola:"
    putStrLn "------------------------------------------------------------------"
    numCola <- colaAleatoria
    print (inserta "Jugador" (colaTienda numCola))
    threadDelay 3000000
    if numCola == 1 then do 
        clearScreen
        putStrLn "------------------------------------------------------------------"
        putStrLn "                          Cola:"
        putStrLn "------------------------------------------------------------------"
        print (inserta "Jugador" vacia)
        threadDelay 3000000
        tienda

    else if numCola == 2 then do 

        clearScreen
        putStrLn "------------------------------------------------------------------"
        putStrLn "                          Cola:"
        putStrLn "------------------------------------------------------------------"
        print (head(drop 1(reverse(vaciarCola numCola (inserta "Jugador" (colaTienda numCola)) []))))
        threadDelay 3000000
        
        clearScreen
        putStrLn "------------------------------------------------------------------"
        putStrLn "                          Cola:"
        putStrLn "------------------------------------------------------------------"
        print (inserta "Jugador" vacia)
        threadDelay 3000000
        tienda

    else if numCola == 3 then do 

        clearScreen
        putStrLn "------------------------------------------------------------------"
        putStrLn "                          Cola:"
        putStrLn "------------------------------------------------------------------"
        print (head(drop 1(reverse(vaciarCola numCola (inserta "Jugador" (colaTienda numCola)) []))))
        threadDelay 3000000
        
        clearScreen
        putStrLn "------------------------------------------------------------------"
        putStrLn "                          Cola:"
        putStrLn "------------------------------------------------------------------"
        print (head(drop 2(reverse(vaciarCola numCola (inserta "Jugador" (colaTienda numCola)) []))))
        threadDelay 3000000

        clearScreen
        putStrLn "------------------------------------------------------------------"
        putStrLn "                          Cola:"
        putStrLn "------------------------------------------------------------------"
        print (inserta "Jugador" vacia)
        threadDelay 3000000
        tienda

    else if numCola == 4 then do

        clearScreen
        putStrLn "------------------------------------------------------------------"
        putStrLn "                          Cola:"
        putStrLn "------------------------------------------------------------------"
        print (head(drop 1(reverse(vaciarCola numCola (inserta "Jugador" (colaTienda numCola)) []))))
        threadDelay 3000000

        clearScreen
        putStrLn "------------------------------------------------------------------"
        putStrLn "                          Cola:"
        putStrLn "------------------------------------------------------------------"
        print (head(drop 2(reverse(vaciarCola numCola (inserta "Jugador" (colaTienda numCola)) []))))
        threadDelay 3000000

        clearScreen
        putStrLn "------------------------------------------------------------------"
        putStrLn "                          Cola:"
        putStrLn "------------------------------------------------------------------"
        print (head(drop 3(reverse(vaciarCola numCola (inserta "Jugador" (colaTienda numCola)) []))))
        threadDelay 3000000

        clearScreen
        putStrLn "------------------------------------------------------------------"
        putStrLn "                          Cola:"
        putStrLn "------------------------------------------------------------------"
        print (inserta "Jugador" vacia)
        threadDelay 3000000
        tienda
    else do
        tienda

tienda :: IO()
tienda = do
    
    clearScreen
    putStrLn "------------------------------------------------------------------"
    putStrLn "- Tendero: ¿Qué necesitas muchacho?"
    putStrLn "------------------------------------------------------------------"
    putStrLn "                  1. ¿Qué tienes a la venta?"
    putStrLn "                  2. Me gustaría venderte algunas cosillas..."
    putStrLn "                  3. Yo ya me iba"
    putStrLn "------------------------------------------------------------------"
    opc <- getLine
    if head opc=='1' then do

        clearScreen
        putStrLn "------------------------------------------------------------------"
        putStrLn "- Tendero: ¡Todo lo que ves! ¿Te interesa algo?"
        putStrLn "------------------------------------------------------------------"
        putStrLn "                  1. Pico - 20$"
        putStrLn "                  2. Tirachinas - 5$"
        putStrLn "                  3. Poción de Salud - 50$"
        putStrLn "------------------------------------------------------------------"
        opc2 <- getLine

-----------------------------------------------    
    --Menú de compra del 1º objeto (Pico)
------------------------------------------------

        if head opc2=='1' then do
            clearScreen
            putStrLn "-------------------------------------------------------------------------------"
            putStr ("- Tendero: "++(head (descripcionItems 1))++" cuesta ")
            print (precio picotienda)
            putStrLn " doblones."
            putStrLn "-------------------------------------------------------------------------------"
            putStrLn "                  1. ¡Me lo llevo!"
            putStrLn "                  2. Creo que puedo sobrevivir sin esa basura..."
            putStrLn "-------------------------------------------------------------------------------"
            opc3 <- getLine
            if head opc3=='1' then do
            
            clearScreen
            putStrLn "-------------------------------------------------------------------------------"
            putStrLn "- Tendero: ¡Ahí tienes!"
            putStrLn "-------------------------------------------------------------------------------"
            restaDinero 20
            threadDelay 2000000
            putStrLn "         - Pico añadido al inventario, ahora puedes picar en la mina -"
            threadDelay 3000000
            tienda
            else if head opc3=='2' then do
                clearScreen
                putStrLn "-------------------------------------------------------------------------------"
                putStrLn "- Tendero: ¡Largo!"
                putStrLn "-------------------------------------------------------------------------------"
                threadDelay 2000000
                pueblo
            else do
                clearScreen
                putStrLn "-------------------------------------------------------------------------------"
                putStrLn "- Tendero: ¿Qué dices? ¡No se te entiende!"
                putStrLn "-------------------------------------------------------------------------------"
                threadDelay 2000000
                tienda
------------------------------------------------    
    --Menú de compra del 2º objeto (Tirachinas)
-------------------------------------------------
        else if head opc2=='2' then do
            clearScreen
            putStrLn "--------------------------------------------------------------------------------------------------"
            putStr ("- Tendero: "++(head (descripcionItems 2))++" cuesta ")
            print (precio tirachinas)
            putStrLn " doblones."
            putStrLn "--------------------------------------------------------------------------------------------------"
            putStrLn "                  1. ¡Me lo llevo!"
            putStrLn "                  2. Creo que puedo sobrevivir sin esa basura..."
            putStrLn "--------------------------------------------------------------------------------------------------"
            opc3 <- getLine
            if head opc3=='1' then do
                clearScreen
                putStrLn "-------------------------------------------------------------------------------"
                putStrLn "- Tendero: Ahí tienes, son 5 doblones."
                putStrLn "-------------------------------------------------------------------------------"
                threadDelay 2000000
                restaDinero 5
                putStrLn "                 - Tirachinas añadido al inventario - "
                putStrLn "          - Ahora podrás cazar con más probabilidad de éxito -"
                threadDelay 3000000
                pueblo
            else if head opc2=='2' then do
                clearScreen
                putStrLn "-------------------------------------------------------------------------------"
                putStrLn "- Tendero: ¡Largo!"
                putStrLn "-------------------------------------------------------------------------------"
                threadDelay 2000000
                pueblo
            else do
                clearScreen
                putStrLn "-------------------------------------------------------------------------------"
                putStrLn "- Tendero: ¿Qué dices? ¡No se te entiende!"
                putStrLn "-------------------------------------------------------------------------------"
                threadDelay 2000000
                tienda 
        else if head opc2=='3' then do
            clearScreen
            putStrLn "-------------------------------------------------------------------------------"
            putStrLn ("- Tendero: "++(head (descripcionItems 3)))
            putStrLn "-------------------------------------------------------------------------------"
            threadDelay 2000000
            tienda
        else do
            clearScreen
            putStrLn "-------------------------------------------------------------------------------"
            putStrLn "- Tendero: ¿Qué dices? ¡No se te entiende!"
            putStrLn "-------------------------------------------------------------------------------"
            threadDelay 2000000
            tienda

    else if head opc=='2' then do
            clearScreen
            putStrLn "-------------------------------------------------------------------------------"
            putStrLn "- Tendero: ¿Que me traes? Puedo comprarte cobre,plata,oro o carne."
            putStrLn "-------------------------------------------------------------------------------"
            putStrLn "                  1. Me gustaría venderte cobre"
            putStrLn "                  2. Me gustaría venderte plata"
            putStrLn "                  3. Me gustaría venderte oro"
            putStrLn "                  4. Me gustaría venderte carne"
            putStrLn "-------------------------------------------------------------------------------"
            opc2 <- getLine
            if head opc2 =='1' then do
                clearScreen
                putStrLn "-------------------------------------------------------------------------------"
                putStrLn "- Tendero: ¿Cuanto tienes? Te daré 2 doblones por cada pieza de cobre"
                putStrLn "-------------------------------------------------------------------------------"
                cob <- getLine
                clearScreen
                putStrLn "-------------------------------------------------------------------------------"
                putStrLn "- Tendero: ¡Está bien! Aquí tienes"
                putStrLn "-------------------------------------------------------------------------------"
                threadDelay 2000000
                sumaDinero 2
                restaCobre 1
                pueblo
            else if head opc2 == '2' then do
                clearScreen
                putStrLn "-------------------------------------------------------------------------------"
                putStrLn "- Tendero: ¿Cuanto tienes? Te daré 4 doblones por cada pieza de plata"
                putStrLn "-------------------------------------------------------------------------------"
                cob <- getLine
                clearScreen
                putStrLn "-------------------------------------------------------------------------------"
                putStrLn "- Tendero: ¡Está bien! Aquí tienes"
                putStrLn "-------------------------------------------------------------------------------"
                threadDelay 2000000
                sumaDinero 4
                restaPlata 1
                pueblo
            else if head opc2 == '3' then do
                clearScreen
                putStrLn "-------------------------------------------------------------------------------"
                putStrLn "- Tendero: ¿Cuanto tienes? Te daré 6 doblones por cada pieza de oro"
                putStrLn "-------------------------------------------------------------------------------"
                cob <- getLine
                clearScreen
                putStrLn "-------------------------------------------------------------------------------"
                putStrLn "- Tendero: ¡Está bien! Aquí tienes"
                putStrLn "-------------------------------------------------------------------------------"
                threadDelay 2000000
                sumaDinero 6
                restaOro 1
                pueblo
            else if head opc2 == '4' then do
                clearScreen
                putStrLn "-------------------------------------------------------------------------------"
                putStrLn "- Tendero: ¿Cuanto tienes? Te daré 5 doblones por cada trozo de carne"
                putStrLn "-------------------------------------------------------------------------------"
                cob <- getLine
                clearScreen
                putStrLn "-------------------------------------------------------------------------------"
                putStrLn "- Tendero: ¡Está bien! Aquí tienes"
                putStrLn "-------------------------------------------------------------------------------"
                threadDelay 2000000
                sumaDinero 5
                restaCarne 1
                pueblo
                
            else do 
                putStrLn "-------------------------------------------------------------------------------"
                putStrLn "- Tendero: Si no tienes nada vete, tengo muchos clientes a la cola..."
                putStrLn "-------------------------------------------------------------------------------"
                threadDelay 2000000
                pueblo
    else if head opc=='3' then do 
        clearScreen
        putStrLn "-------------------------------------------------------------------------------"
        putStrLn "- Tendero: ¡Largo!"
        putStrLn "-------------------------------------------------------------------------------"
        threadDelay 2000000
        pueblo 
    else do 
        clearScreen
        putStrLn "-------------------------------------------------------------------------------"
        putStrLn "- Tendero: ¿Qué dices? ¡No se te entiende!"
        putStrLn "-------------------------------------------------------------------------------"
        threadDelay 2000000
        tienda
-- ---------------------------------------------------------------------
-- Guardar Partida
-- ---------------------------------------------------------------------
guardarPartida :: IO()
guardarPartida = do 
    clearScreen
    --Cargamos la partida actual y los 3 slots:
    readsg <- readFile "savegames/temp.txt"
    readsg1 <- readFile "savegames/sg1.txt"
    readsg2 <- readFile "savegames/sg2.txt"
    readsg3 <- readFile "savegames/sg3.txt"

    let savegame = parseCSV "savegames/temp.txt" readsg
        datos = case savegame of
          (Right lineas) -> lineas
          _ -> []

    let savegame1 = parseCSV "savegames/sg1.txt" readsg1
        datos1 = case savegame1 of
          (Right lineas) -> lineas
          _ -> []
    let savegame2 = parseCSV "savegames/sg2.txt" readsg2
        datos2 = case savegame2 of
          (Right lineas) -> lineas
          _ -> []
    let savegame3 = parseCSV "savegames/sg3.txt" readsg3
        datos3 = case savegame3 of
          (Right lineas) -> lineas
          _ -> []

-- Aquí sacamos los nombres y niveles de todos los slots para
-- mostrarlos por pantalla:
    let nombre1 = head (head datos1)
    let nombre2 = head (head datos2)
    let nombre3 = head (head datos3)

    let nivel1 = head (head (drop 1 datos1))
    let nivel2 = head (head (drop 1 datos2))
    let nivel3 = head (head (drop 1 datos3))

-- Estos son los stats del fichero temporal, (nuestra partida):
    let nombre = head (head datos)
    let nivel = head (head (drop 1 datos))
    let dinero = head (head (drop 2 datos))
    let pico = head (head (drop 3 datos))
    let cobre = head (head (drop 4 datos))
    let plata = head (head (drop 5 datos))
    let oro = head (head (drop 6 datos))
    let carne = head (head (drop 7 datos))

-- Imprimimos por pantalla los 3 slots disponibles:

    putStrLn "------------------------------------------------------------------"
    putStrLn "       ¿En qué slot te gustaría guardar la partida?"
    putStrLn " CUIDADO: Si el slot tiene una partida guardada, se sobrescribirá"
    putStrLn "------------------------------------------------------------------"
    putStr "1. - " 
    putStr (nombre1 ++ " nvl." ++ nivel1)
    putStrLn " -"
    putStr "2. - " 
    putStr (nombre2 ++ " nvl." ++ nivel2)
    putStrLn " -"
    putStr "3. - " 
    putStr (nombre3 ++ " nvl." ++ nivel3)
    putStrLn " -"
    putStrLn "------------------------------------------------------------------"
    opc <- getLine
    let cadena = nombre++"\n"++nivel++"\n"++dinero++"\n"++pico++"\n"++cobre++"\n"++plata++"\n"++oro++"\n"++carne
    if (head opc=='1') then do

        -- Remplazamos el slot elegido con los datos del fichero temporal:
        
        writeFile "savegames/sg1.txt" cadena
        clearScreen
        putStr "Guardando Partida"
        threadDelay 1000000
        putStr "."
        threadDelay 1000000
        putStr "."
        threadDelay 1000000
        putStrLn "."
        main
    else if (head opc=='2') then do 
        writeFile "savegames/sg2.txt" cadena
        clearScreen
        putStr "Guardando Partida"
        threadDelay 1000000
        putStr "."
        threadDelay 1000000
        putStr "."
        threadDelay 1000000
        putStrLn "."
        main
    else if (head opc=='3') then do 
        writeFile "savegames/sg3.txt" cadena
        clearScreen
        putStr "Guardando Partida"
        threadDelay 1000000
        putStr "."
        threadDelay 1000000
        putStr "."
        threadDelay 1000000
        putStrLn "."
        main
    else do 
        putStrLn "Comando Incorrecto"
        threadDelay 1000000
        menuInGame
-- ---------------------------------------------------------------------
-- Menú Ingame
-- ---------------------------------------------------------------------
menuInGame :: IO()
menuInGame = do 
    clearScreen 
    putStrLn "------------------------------------------------------------------"
    getName
    putStrLn "------------------------------------------------------------------"
    putStrLn "                     1. Ir al Pueblo"
    putStrLn "                     2. Ir a la Montaña"
    putStrLn "                     3. Ir a la Mina"
    putStrLn "                     4. Ver inventario"
    putStrLn "                     5. Salir al menú principal"
    putStrLn "------------------------------------------------------------------\n"
    opc1 <- getLine
    if head opc1=='1' then do
        clearScreen
        putStrLn "------------------------------------------------------------------"
        putStrLn "Tras caminar por un largo tiempo llegas a un pequeño pueblo..."
        putStrLn "------------------------------------------------------------------"
        threadDelay 2000000
        pueblo
    else if head opc1=='2' then do
        montana
    else if head opc1=='3' then do 
        mina 
    else if head opc1=='4' then do 
        inventario
    else if head opc1=='5' then do
        clearScreen
        putStrLn "------------------------------------------------------------------------------------"
        putStrLn "  ¿Estás seguro de que quieres guardar la partida y salir al menú principal? (s/n)"
        putStrLn "------------------------------------------------------------------------------------"
        opc2 <- getLine
        if (head opc2=='s' || head opc2=='S') then do 
            guardarPartida 
        else if (head opc2=='n' || head opc2=='N') then do 
            menuInGame
        else do 
            putStrLn "Acción incorrecta"
            threadDelay 1000000
            menuInGame
    else do 
        putStrLn "Acción incorrecta"
        threadDelay 1000000
        menuInGame

-- ---------------------------------------------------------------------
-- Cargar Partida
-- ---------------------------------------------------------------------
cargarPartida :: IO ()
cargarPartida = do
    clearScreen

    -- Leemos los 3 slots:
    readsg1 <- readFile "savegames/sg1.txt"
    readsg2 <- readFile "savegames/sg2.txt"
    readsg3 <- readFile "savegames/sg3.txt"

    -- Los parseamos para convertirlo en una lista de listas, y le eliminamos
    -- el 'Right' que se genera a la izquierda de los datos
    -- Cada archivo datos1,datos2... tienen el formato: 
    --
    -- [[nombre],[nivel],[dinero],[pico],[cobre],[plata],[oro],[carne]]
    --

    let savegame1 = parseCSV "savegames/sg1.txt" readsg1
        datos1 = case savegame1 of
          (Right lineas) -> lineas
          _ -> []
    let savegame2 = parseCSV "savegames/sg2.txt" readsg2
        datos2 = case savegame2 of
          (Right lineas) -> lineas
          _ -> []
    let savegame3 = parseCSV "savegames/sg3.txt" readsg3
        datos3 = case savegame3 of
          (Right lineas) -> lineas
          _ -> []

-- Aquí sacamos cada uno de los valores de las partidas:

-- nombre
    let nombre1 = head (head datos1)
    let nombre2 = head (head datos2)
    let nombre3 = head (head datos3)
-- nivel
    let nivel1 = head (head (drop 1 datos1))
    let nivel2 = head (head (drop 1 datos2))
    let nivel3 = head (head (drop 1 datos3))
-- dinero
    let dinero1 = head (head (drop 2 datos1))
    let dinero2 = head (head (drop 2 datos2))
    let dinero3 = head (head (drop 2 datos3))
-- pico
    let pico1 = head (head (drop 3 datos1))
    let pico2 = head (head (drop 3 datos2))
    let pico3 = head (head (drop 3 datos3))
-- cobre
    let cobre1 = head (head (drop 4 datos1))
    let cobre2 = head (head (drop 4 datos2))
    let cobre3 = head (head (drop 4 datos3))
-- plata
    let plata1 = head (head (drop 5 datos1))
    let plata2 = head (head (drop 5 datos2))
    let plata3 = head (head (drop 5 datos3))
-- oro
    let oro1 = head (head (drop 6 datos1))
    let oro2 = head (head (drop 6 datos2))
    let oro3 = head (head (drop 6 datos3))
-- carne
    let carne1 = head (head (drop 7 datos1))
    let carne2 = head (head (drop 7 datos2))
    let carne3 = head (head (drop 7 datos3))
    
    -- Menú visual con los 3 slots disponibles para cargar:

    putStrLn "------------------------------------------------------------------"
    putStrLn "                      Cargar Partida:"
    putStrLn "------------------------------------------------------------------"
    putStr "1. - " 
    putStr (nombre1 ++ " nvl." ++ nivel1)
    putStrLn " -"
    putStr "2. - " 
    putStr (nombre2 ++ " nvl." ++ nivel2)
    putStrLn " -"
    putStr "3. - " 
    putStr (nombre3 ++ " nvl." ++ nivel3)
    putStrLn " -"
    putStrLn "------------------------------------------------------------------"
    opc <- getLine
    if (head opc=='1') then do
        -- Guardamos en el archivo temporal los datos del slot que hemos elegido
        -- cargar.

        let cadena = nombre1++"\n"++nivel1++"\n"++dinero1++"\n"++pico1++"\n"++cobre1++"\n"++plata1++"\n"++oro1++"\n"++carne1
        writeFile "savegames/temp.txt" cadena
        clearScreen
        putStr "Cargando Partida"
        threadDelay 1000000
        putStr "."
        threadDelay 1000000
        putStr "."
        threadDelay 1000000
        putStrLn "."
        menuInGame
    else if (head opc=='2') then do 
        let cadena = nombre2++"\n"++nivel2++"\n"++dinero2++"\n"++pico2++"\n"++cobre2++"\n"++plata2++"\n"++oro2++"\n"++carne2
        writeFile "savegames/temp.txt" cadena
        clearScreen
        putStr "Cargando Partida"
        threadDelay 1000000
        putStr "."
        threadDelay 1000000
        putStr "."
        threadDelay 1000000
        putStrLn "."
        menuInGame
    else if (head opc=='3') then do 
        let cadena = nombre3++"\n"++nivel3++"\n"++dinero3++"\n"++pico3++"\n"++cobre3++"\n"++plata3++"\n"++oro3++"\n"++carne3
        writeFile "savegames/temp.txt" cadena
        clearScreen
        putStr "Cargando Partida"
        threadDelay 1000000
        putStr "."
        threadDelay 1000000
        putStr "."
        threadDelay 1000000
        putStrLn "."
        menuInGame
    else do 
        putStrLn "Comando Incorrecto"
        threadDelay 1000000
        main
    
-- ---------------------------------------------------------------------
-- Nueva Partida: Crea el archivo temporal con nuestro nombre de jugador
-- y el resto de campos en default:
--
-- [["Nombre"],["0"],["0"],["False"],["0"],["0"],["0"],["0"]]
--
-- Siendo cada slot:
--
-- [[nombre],[nivel],[dinero],[pico],[cobre],[plata],[oro],[carne]]
-- 
-- ---------------------------------------------------------------------
nuevaPartida :: IO()
nuevaPartida = do
    clearScreen 
    putStr "Nombre del personaje: "
    user <- getLine
    clearScreen
    let cadena = user++"\n"++"1"++"\n"++"0"++"\n"++"False"++"\n"++"0"++"\n"++"0"++"\n"++"0"++"\n"++"0"
    writeFile "savegames/temp.txt" cadena
    clearScreen
    putStrLn "------------------------------------------------------------------"
    putStrLn ("Bienvenido al reino de Eqens, " ++ user)
    putStrLn "------------------------------------------------------------------"
    threadDelay 3000000
    menuInGame

-- ---------------------------------------------------------------------
-- Mina: Función para minar, se necesita un pico para ello (pico=True)
-- Probabilidad de los distintos materiales:
-- Nada: 6/11, Cobre: 4/11, Plata: 3/11, Oro: 2/11
-- ---------------------------------------------------------------------


mina :: IO()
mina = do
    clearScreen
    putStrLn "-----------------------------------------------------------------------"
    putStrLn "Llegas a una oscura mina y te adentras en lo más profundo de ella..."
    putStrLn "-----------------------------------------------------------------------"
    threadDelay 2000000

    if (pico) then do
        aleatorio<-numeroAleatorio
        clearScreen
        putStrLn "----------------------------------------------------------------------------------------------"
        putStrLn "Te encuentras un gran pedrusco reluciente en la oscuridad y empiezas a golpearlo con tu pico"
        putStrLn "----------------------------------------------------------------------------------------------"
        threadDelay 3000000
        putStr "Minando"
        threadDelay 1000000
        putStr "."
        threadDelay 1000000
        putStr "."
        threadDelay 1000000
        putStr ". "
        if ((aleatorio>=0) && (aleatorio<=5)) then do
            putStrLn "No has encontrado nada, mala suerte"
            threadDelay 2000000
            menuInGame
        else if (aleatorio>5 && aleatorio<=8) then do 
            putStrLn "¡Enhorabuena! Has encontrado cobre"
            sumaCobre 1
            threadDelay 2000000
            menuInGame
        else if (aleatorio>8 && aleatorio<=10) then do
            putStrLn "¡Enhorabuena! Has encontrado plata"
            sumaPlata 1
            threadDelay 2000000
            menuInGame
        else do
            putStrLn "¡Enhorabuena! Has encontrado oro"
            sumaOro 1
            threadDelay 2000000
            menuInGame
        
    else do 
        putStrLn "Minero: ¡Necesitas un pico para minar aquí muchacho!"
        threadDelay 1000000
        putStrLn "Minero: Seguro que en la tienda del pueblo pueden ayudarte"
        threadDelay 3000000
        menuInGame

-- ---------------------------------------------------------------------
-- Pueblo
-- ---------------------------------------------------------------------    
pueblo :: IO()
pueblo = do 
    clearScreen
    putStrLn "------------------------------------------------------------------"
    putStrLn "Te encuentras en la plaza central... ¿A dónde te dirijes?"
    putStrLn "------------------------------------------------------------------"
    putStrLn "                     1. Ir a la tienda"
    putStrLn "                     2. Ir al ayuntamiento"
    putStrLn "                     3. Hablar con el guardia"
    putStrLn "                     4. Hablar con el bufón"
    putStrLn "                     5. Hablar con el gigante"
    putStrLn "                     6. Hablar con frutero malhumorado"
    putStrLn "                     7. Salir del pueblo"
    putStrLn "------------------------------------------------------------------\n"
    opc1 <- getLine
    if head opc1=='1' then do
        cola
    else if head opc1=='2' then do
        ayuntamiento
    else if head opc1=='3' then do 
        guardia
    else if head opc1=='4' then do 
        bufon
    else if head opc1=='5' then do 
        gigante
    else if head opc1=='6' then do
        frutero
    else if head opc1=='7' then do 
        menuInGame
    else do 
        putStrLn "No te he entendido"
        threadDelay 1000000
        pueblo

-- ---------------------------------------------------------------------
-- Menú Principal
-- ---------------------------------------------------------------------
main :: IO()
main = do 
    hSetBuffering stdout NoBuffering
    clearScreen
    
    putStrLn "------------------------------------------------------------------"
    putStrLn "   Bienvenido a EQENS - Una aventura conversacional en Haskell"
    putStrLn "------------------------------------------------------------------"
    putStrLn "                     1. Nueva Partida"
    putStrLn "                     2. Cargar Partida"
    putStrLn "                     3. Salir"
    putStrLn "------------------------------------------------------------------\n"
    opc <- getLine
    if head opc=='1' then do
        nuevaPartida
    else if head opc=='2' then do
        cargarPartida
    else if head opc=='3' then do
        forkIO (exitWith (ExitFailure 45))
        threadDelay 1000000
        putStrLn "¡Vuelve Pronto!"
    else do
        putStrLn "No se puede reconocer el comando, vuelve a intentarlo"
        threadDelay 1000000
        clearScreen 
        main
-- ---------------------------------------------------------------------