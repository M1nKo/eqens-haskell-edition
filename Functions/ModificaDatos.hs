module Functions.ModificaDatos(getName,
                    sumaNivel,restaNivel,
                    sumaDinero,restaDinero,
                    sumaCobre,restaCobre,
                    sumaPlata,restaPlata,
                    sumaOro,restaOro,
                    sumaCarne,restaCarne,
                    actualizaDatos)where
import Text.CSV

-- ---------------------------------------------------------------------
-- getName: imprime el nombre y el nivel de la partida cargada
-- ---------------------------------------------------------------------
getName :: IO() 
getName = do
    readsg <- readFile "savegames/temp.txt"
    let savegame = parseCSV "savegames/temp.txt" readsg
        datos = case savegame of
          (Right lineas) -> lineas
          _ -> []
    
    let nombre = head (head datos)
    let nivel = head (head (drop 1 datos))

    print (nombre ++ " lvl."++nivel)

---------------------------------------------------------------------
-- Por cada valor numérico guardado en la partida:
-- [nivel,cobre,plata,oro,carne]
-- Hay una función para incrementarlo o decrementarlo.
---------------------------------------------------------------------
-- Función que suma nivel y guarda el resultado en tempMoney.txt
---------------------------------------------------------------------
sumaNivel :: Int -> IO()
sumaNivel n = do 
    readsg <- readFile "savegames/temp.txt"
    let savegame = parseCSV "savegames/temp.txt" readsg
        datos = case savegame of
          (Right lineas) -> lineas
          _ -> []
    
    let nombre = head (head datos)
    let nivel = head (head (drop 1 datos))
    let dinero = head (head (drop 2 datos))
    let pico = head (head (drop 3 datos))
    let cobre = head (head (drop 4 datos))
    let plata = head (head (drop 5 datos))
    let oro = head (head (drop 6 datos))
    let carne = head (head (drop 7 datos))

    let nivelmodificado = show((read nivel :: Int)+n)
    let cadena = nombre++"\n"++nivelmodificado++"\n"++dinero++"\n"++pico++"\n"++cobre++"\n"++plata++"\n"++oro++"\n"++carne
    writeFile "savegames/temp2.txt" cadena
    actualizaDatos

---------------------------------------------------------------------
-- Función que resta nivel y guarda el resultado en tempMoney.txt
---------------------------------------------------------------------
restaNivel :: Int -> IO()
restaNivel n = do 
    readsg <- readFile "savegames/temp.txt"
    let savegame = parseCSV "savegames/temp.txt" readsg
        datos = case savegame of
          (Right lineas) -> lineas
          _ -> []
    
    let nombre = head (head datos)
    let nivel = head (head (drop 1 datos))
    let dinero = head (head (drop 2 datos))
    let pico = head (head (drop 3 datos))
    let cobre = head (head (drop 4 datos))
    let plata = head (head (drop 5 datos))
    let oro = head (head (drop 6 datos))
    let carne = head (head (drop 7 datos))

    let nivelmodificado = show((read nivel :: Int)-n)
    let cadena = nombre++"\n"++nivelmodificado++"\n"++dinero++"\n"++pico++"\n"++cobre++"\n"++plata++"\n"++oro++"\n"++carne
    writeFile "savegames/temp2.txt" cadena
    actualizaDatos
---------------------------------------------------------------------
-- Función que suma cobre y guarda el resultado en tempMoney.txt
---------------------------------------------------------------------
sumaCobre :: Int -> IO()
sumaCobre n = do 
    readsg <- readFile "savegames/temp.txt"
    let savegame = parseCSV "savegames/temp.txt" readsg
        datos = case savegame of
          (Right lineas) -> lineas
          _ -> []
    
    let nombre = head (head datos)
    let nivel = head (head (drop 1 datos))
    let dinero = head (head (drop 2 datos))
    let pico = head (head (drop 3 datos))
    let cobre = head (head (drop 4 datos))
    let plata = head (head (drop 5 datos))
    let oro = head (head (drop 6 datos))
    let carne = head (head (drop 7 datos))

    let cobremodificado = show((read cobre :: Int)+n)
    let cadena = nombre++"\n"++nivel++"\n"++dinero++"\n"++pico++"\n"++cobremodificado++"\n"++plata++"\n"++oro++"\n"++carne
    writeFile "savegames/temp2.txt" cadena
    actualizaDatos

---------------------------------------------------------------------
-- Función que resta cobre y guarda el resultado en tempMoney.txt
---------------------------------------------------------------------
restaCobre :: Int -> IO()
restaCobre n = do 
    readsg <- readFile "savegames/temp.txt"
    let savegame = parseCSV "savegames/temp.txt" readsg
        datos = case savegame of
          (Right lineas) -> lineas
          _ -> []
    
    let nombre = head (head datos)
    let nivel = head (head (drop 1 datos))
    let dinero = head (head (drop 2 datos))
    let pico = head (head (drop 3 datos))
    let cobre = head (head (drop 4 datos))
    let plata = head (head (drop 5 datos))
    let oro = head (head (drop 6 datos))
    let carne = head (head (drop 7 datos))

    let cobremodificado = show((read cobre :: Int)-n)
    let cadena = nombre++"\n"++nivel++"\n"++dinero++"\n"++pico++"\n"++cobremodificado++"\n"++plata++"\n"++oro++"\n"++carne
    writeFile "savegames/temp2.txt" cadena
    actualizaDatos
---------------------------------------------------------------------
-- Función que suma plata y guarda el resultado en tempMoney.txt
---------------------------------------------------------------------
sumaPlata :: Int -> IO()
sumaPlata n = do 
    readsg <- readFile "savegames/temp.txt"
    let savegame = parseCSV "savegames/temp.txt" readsg
        datos = case savegame of
          (Right lineas) -> lineas
          _ -> []
    
    let nombre = head (head datos)
    let nivel = head (head (drop 1 datos))
    let dinero = head (head (drop 2 datos))
    let pico = head (head (drop 3 datos))
    let cobre = head (head (drop 4 datos))
    let plata = head (head (drop 5 datos))
    let oro = head (head (drop 6 datos))
    let carne = head (head (drop 7 datos))

    let platamodificada = show((read plata :: Int)+n)
    let cadena = nombre++"\n"++nivel++"\n"++dinero++"\n"++pico++"\n"++cobre++"\n"++platamodificada++"\n"++oro++"\n"++carne
    writeFile "savegames/temp2.txt" cadena
    actualizaDatos
---------------------------------------------------------------------
-- Función que resta plata y guarda el resultado en tempMoney.txt
---------------------------------------------------------------------
restaPlata :: Int -> IO()
restaPlata n = do 
    readsg <- readFile "savegames/temp.txt"
    let savegame = parseCSV "savegames/temp.txt" readsg
        datos = case savegame of
          (Right lineas) -> lineas
          _ -> []
    
    let nombre = head (head datos)
    let nivel = head (head (drop 1 datos))
    let dinero = head (head (drop 2 datos))
    let pico = head (head (drop 3 datos))
    let cobre = head (head (drop 4 datos))
    let plata = head (head (drop 5 datos))
    let oro = head (head (drop 6 datos))
    let carne = head (head (drop 7 datos))

    let platamodificada = show((read plata :: Int)-n)
    let cadena = nombre++"\n"++nivel++"\n"++dinero++"\n"++pico++"\n"++cobre++"\n"++platamodificada++"\n"++oro++"\n"++carne
    writeFile "savegames/temp2.txt" cadena
    actualizaDatos
---------------------------------------------------------------------
-- Función que suma oro y guarda el resultado en tempMoney.txt
---------------------------------------------------------------------
sumaOro :: Int -> IO()
sumaOro n = do 
    readsg <- readFile "savegames/temp.txt"
    let savegame = parseCSV "savegames/temp.txt" readsg
        datos = case savegame of
          (Right lineas) -> lineas
          _ -> []
    
    let nombre = head (head datos)
    let nivel = head (head (drop 1 datos))
    let dinero = head (head (drop 2 datos))
    let pico = head (head (drop 3 datos))
    let cobre = head (head (drop 4 datos))
    let plata = head (head (drop 5 datos))
    let oro = head (head (drop 6 datos))
    let carne = head (head (drop 7 datos))

    let oromodificado = show((read oro :: Int)+n)
    let cadena = nombre++"\n"++nivel++"\n"++dinero++"\n"++pico++"\n"++cobre++"\n"++plata++"\n"++oromodificado++"\n"++carne
    writeFile "savegames/temp2.txt" cadena
    actualizaDatos

---------------------------------------------------------------------
-- Función que resta oro y guarda el resultado en tempMoney.txt
---------------------------------------------------------------------
restaOro :: Int -> IO()
restaOro n = do 
    readsg <- readFile "savegames/temp.txt"
    let savegame = parseCSV "savegames/temp.txt" readsg
        datos = case savegame of
          (Right lineas) -> lineas
          _ -> []
    
    let nombre = head (head datos)
    let nivel = head (head (drop 1 datos))
    let dinero = head (head (drop 2 datos))
    let pico = head (head (drop 3 datos))
    let cobre = head (head (drop 4 datos))
    let plata = head (head (drop 5 datos))
    let oro = head (head (drop 6 datos))
    let carne = head (head (drop 7 datos))

    let oromodificado = show((read oro :: Int)-n)
    let cadena = nombre++"\n"++nivel++"\n"++dinero++"\n"++pico++"\n"++cobre++"\n"++plata++"\n"++oromodificado++"\n"++carne
    writeFile "savegames/temp2.txt" cadena
    actualizaDatos
---------------------------------------------------------------------
-- Función que suma carne y guarda el resultado en tempMoney.txt
---------------------------------------------------------------------
sumaCarne :: Int -> IO()
sumaCarne n = do 
    readsg <- readFile "savegames/temp.txt"
    let savegame = parseCSV "savegames/temp.txt" readsg
        datos = case savegame of
          (Right lineas) -> lineas
          _ -> []
    
    let nombre = head (head datos)
    let nivel = head (head (drop 1 datos))
    let dinero = head (head (drop 2 datos))
    let pico = head (head (drop 3 datos))
    let cobre = head (head (drop 4 datos))
    let plata = head (head (drop 5 datos))
    let oro = head (head (drop 6 datos))
    let carne = head (head (drop 7 datos))

    let carnemodificada = show((read carne :: Int)+n)
    let cadena = nombre++"\n"++nivel++"\n"++dinero++"\n"++pico++"\n"++cobre++"\n"++plata++"\n"++oro++"\n"++carnemodificada
    writeFile "savegames/temp2.txt" cadena
    actualizaDatos
---------------------------------------------------------------------
-- Función que resta carne y guarda el resultado en tempMoney.txt
---------------------------------------------------------------------
restaCarne :: Int -> IO()
restaCarne n = do 
    readsg <- readFile "savegames/temp.txt"
    let savegame = parseCSV "savegames/temp.txt" readsg
        datos = case savegame of
          (Right lineas) -> lineas
          _ -> []
    
    let nombre = head (head datos)
    let nivel = head (head (drop 1 datos))
    let dinero = head (head (drop 2 datos))
    let pico = head (head (drop 3 datos))
    let cobre = head (head (drop 4 datos))
    let plata = head (head (drop 5 datos))
    let oro = head (head (drop 6 datos))
    let carne = head (head (drop 7 datos))

    let carnemodificada = show((read carne :: Int)-n)
    let cadena = nombre++"\n"++nivel++"\n"++dinero++"\n"++pico++"\n"++cobre++"\n"++plata++"\n"++oro++"\n"++carnemodificada
    writeFile "savegames/temp2.txt" cadena
    actualizaDatos
---------------------------------------------------------------------
-- Función que suma dinero y guarda el resultado en tempMoney.txt
---------------------------------------------------------------------
sumaDinero :: Int -> IO()
sumaDinero n = do 
    readsg <- readFile "savegames/temp.txt"
    let savegame = parseCSV "savegames/temp.txt" readsg
        datos = case savegame of
          (Right lineas) -> lineas
          _ -> []
    
    let nombre = head (head datos)
    let nivel = head (head (drop 1 datos))
    let dinero = head (head (drop 2 datos))
    let pico = head (head (drop 3 datos))
    let cobre = head (head (drop 4 datos))
    let plata = head (head (drop 5 datos))
    let oro = head (head (drop 6 datos))
    let carne = head (head (drop 7 datos))

    let dineromodificado = show((read dinero :: Int)+n)
    let cadena = nombre++"\n"++nivel++"\n"++dineromodificado++"\n"++pico++"\n"++cobre++"\n"++plata++"\n"++oro++"\n"++carne
    writeFile "savegames/temp2.txt" cadena
    actualizaDatos

---------------------------------------------------------------------
-- Función que resta dinero y guarda el resultado en tempMoney.txt
---------------------------------------------------------------------

restaDinero :: Int -> IO()
restaDinero n = do 
    readsg <- readFile "savegames/temp.txt"
    let savegame = parseCSV "savegames/temp.txt" readsg
        datos = case savegame of
          (Right lineas) -> lineas
          _ -> []
    
    let nombre = head (head datos)
    let nivel = head (head (drop 1 datos))
    let dinero = head (head (drop 2 datos))
    let pico = head (head (drop 3 datos))
    let cobre = head (head (drop 4 datos))
    let plata = head (head (drop 5 datos))
    let oro = head (head (drop 6 datos))
    let carne = head (head (drop 7 datos))
    let dineromodificado = show((read dinero :: Int)-n)
    let cadena = nombre++"\n"++nivel++"\n"++dineromodificado++"\n"++pico++"\n"++cobre++"\n"++plata++"\n"++oro++"\n"++carne
    writeFile "savegames/temp2.txt" cadena
    actualizaDatos

-------------------------------------------------------------------------
-- Función que actualiza los datos de temp.txt con los de temp2.txt
-------------------------------------------------------------------------

actualizaDatos :: IO ()
actualizaDatos = do 
    readsg <- readFile "savegames/temp2.txt"
    let tempactualizado = parseCSV "savegames/temp2.txt" readsg
        datosactualizados = case tempactualizado of
          (Right lineas) -> lineas
          _ -> []

    let nombre = head (head datosactualizados)
    let nivel = head (head (drop 1 datosactualizados))
    let dinero = head (head (drop 2 datosactualizados))
    let pico = head (head (drop 3 datosactualizados))

    let cobre = head (head (drop 4 datosactualizados))
    let plata = head (head (drop 5 datosactualizados))
    let oro = head (head (drop 6 datosactualizados))
    let carne = head (head (drop 7 datosactualizados))

    let cadena = nombre++"\n"++nivel++"\n"++dinero++"\n"++pico++"\n"++cobre++"\n"++plata++"\n"++oro++"\n"++carne
    writeFile "savegames/temp.txt" cadena
