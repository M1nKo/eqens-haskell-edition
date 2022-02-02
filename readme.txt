----------------------------------------------------------------
 - EQENS - Una aventura conversacional en Haskell -
----------------------------------------------------------------

EQENS es una aventura en forma de texto escrita en Haskell,
para avanzar de unos menús a otros simplemente debes escribir
el número de opción que deseas y darle a enter.

Cuando el juego no te dé opciones que elegir no debes escribir
nada, simplemente espera (hay un pequeño delay entre menús).

----------------------------------------------------------------
 - ¿Cómo compilar?
----------------------------------------------------------------

Desde la terminal/cmd debemos acceder al directorio
del juego y escribir el siguiente comando:

$ ghc Main.hs -o Main

Se generará un Main.exe que podemos ejecutar.

Si en vez de compilarlo quieres interpretarlo en ghci desde
VSCode deberás llamar a la función main una vez interpretado.
