module Util where

-- | @alinearDerecha n s@ agrega espacios a la izquierda de @s@ hasta que su longitud sea @n@.
-- Si @s@ ya tiene longitud @>= n@, devuelve @s@.
-- alinearDerecha :: Int -> String -> String

alinearDerecha n s = espacios ++ s
     where
        m = n-(length s)        -- calculamos cuantos caracteres nos hacen falta
        t = [ x | x <- [1..m] ] -- generamos una lista por compresion de esos caracteres a foldear
        f x res = " " ++ res    -- lambda para el fold, simplemente concatena los espacios
        espacios = foldr f "" t -- generamos los espacios concatenando la lista por compresion

-- | Dado un índice y una función, actualiza el elemento en la posición del índice
-- aplicando la función al valor actual. Si el índice está fuera de los límites
-- de la lista, devuelve la lista sin cambios.
-- El primer elemento de la lista es el índice 0.
actualizarElem :: Int -> (a -> a) -> [a] -> [a]
actualizarElem n f xs = error "COMPLETAR EJERCICIO 2"

-- | infinito positivo (Haskell no tiene literal para +infinito)
infinitoPositivo :: Float
infinitoPositivo = 1 / 0

-- | infinito negativo (Haskell no tiene literal para -infinito)
infinitoNegativo :: Float
infinitoNegativo = -(1 / 0)
