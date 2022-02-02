module Functions.RandomNumber
(numeroAleatorio,colaAleatoria) where



import System.Random -- para números aleatorios
---------------------------------------------------------
-- numeroAleatorio: devuelve un número aleatorio entre
-- 10 y 11, se utiliza para las probabilidades de 
-- encontrar los minerales en la mina y para la
-- probabilidad de cazar un conejo o no en la montaña
---------------------------------------------------------
numeroAleatorio :: IO Int
numeroAleatorio = do
    random <- randomRIO (0,11)
    return (random)   
---------------------------------------------------------
-- colaAleatoria: devuelve un número aleatorio entre
-- 1 y 4, se utiliza para elegir que cola de las 4 
-- posibles se genera al entrar en la tienda.
---------------------------------------------------------
colaAleatoria :: IO Int 
colaAleatoria = do 
    random <- randomRIO (1,4)
    return (random) 
