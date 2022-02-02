module Functions.Guardia(dialogoGuardia)where

-- ---------------------------------------------------------------------
-- Guardia: según el número que le hayamos dicho por teclado nos 
-- hablará sobre una zona del juego u otra.
-- ---------------------------------------------------------------------
dialogoGuardia :: Int -> String
dialogoGuardia n =
    case n of
        4 -> "En la mina puedes conseguir minerales, oro, plata o cobre\n ¡Pero no todos son igual de sencillos de encontrar!"
        n
          | n == 1 -> "El pueblo Melee es el centro de operaciones, puedes hablar\n con los habitantes para ver que tienen entre manos para tí."
          | n == 3 -> "En la montaña podrás intentar cazar algún conejo, si usas\n alguna herramienta como un tirachinas probablemente tengas más\n exitos en tu caza."
          | n == 2 -> "En la tienda del pueblo puedes comprar objetos o vender\n lo que recolectes en la mina o en la montaña."