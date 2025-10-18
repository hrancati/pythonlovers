-- | Un `Histograma` es una estructura de datos que permite contar cuántos valores hay en cada rango.
-- @vacio n (a, b)@ devuelve un histograma vacío con n+2 casilleros:
--
-- * @(-inf, a)@
-- * @[a, a + tamIntervalo)@
-- * @[a + tamIntervalo, a + 2*tamIntervalo)@
-- * ...
-- * @[b - tamIntervalo, b)@
-- * @[b, +inf)@
--
-- `vacio`, `agregar` e `histograma` se usan para construir un histograma.
module Histograma
  ( Histograma, -- No se exportan los constructores
    vacio,
    agregar,
    histograma,
    Casillero (..),
    casMinimo,
    casMaximo,
    casCantidad,
    casPorcentaje,
    casilleros,
  )
where

import Util
import Data.List (zipWith4) --lo agregamos para poder utilizar zipWith4 en ejercicio 6 para definir la función casilleros

data Histograma = Histograma Float Float [Int]
  deriving (Show, Eq)

-- | Inicializa un histograma vacío con @n@ casilleros para representar
-- valores en el rango y 2 casilleros adicionales para los valores fuera del rango.
-- Require que @l < u@ y @n >= 1@.
vacio :: Int -> (Float, Float) -> Histograma
vacio n (l, u) = Histograma l tamanioIntervalo (replicate (n + 2) 0) 
  where 
    tamanioIntervalo = (u - l) / fromIntegral n

-- | Agrega un valor al histograma.
agregar :: Float -> Histograma -> Histograma
agregar y (Histograma i t xs)= Histograma i t (actualizarElem k (+1) xs ) -- modifica la lista del Histograma y suma 1 a la posicion ya calculada
  where 
    k= calcularPosicion y i t (length xs)
    
calcularPosicion:: Float -> Float -> Float-> Int-> Int   
calcularPosicion y i t n | y < i                         = 0         
                         | y >= i + t*fromIntegral (n-2) = n - 1                 
                         | otherwise                     = 1 + floor ((y-i)/t)  


-- | Arma un histograma a partir de una lista de números reales con la cantidad de casilleros y rango indicados.
histograma :: Int -> (Float, Float) -> [Float] -> Histograma
histograma n r = foldr agregar (vacio n r)

-- | Un `Casillero` representa un casillero del histograma con sus límites, cantidad y porcentaje.
-- Invariante: Sea @Casillero m1 m2 c p@ entonces @m1 < m2@, @c >= 0@, @0 <= p <= 100@
data Casillero = Casillero Float Float Int Float
  deriving (Show, Eq)

-- | Mínimo valor del casillero (el límite inferior puede ser @-inf@)
casMinimo :: Casillero -> Float
casMinimo (Casillero m _ _ _) = m

-- | Máximo valor del casillero (el límite superior puede ser @+inf@)
casMaximo :: Casillero -> Float
casMaximo (Casillero _ m _ _) = m

-- | Cantidad de valores en el casillero. Es un entero @>= 0@.
casCantidad :: Casillero -> Int
casCantidad (Casillero _ _ c _) = c

-- | Porcentaje de valores en el casillero respecto al total de valores en el histograma. Va de 0 a 100.
casPorcentaje :: Casillero -> Float
casPorcentaje (Casillero _ _ _ p) = p

-- | Dado un histograma, devuelve la lista de casilleros con sus límites, cantidad y porcentaje.
casilleros :: Histograma -> [Casillero]
casilleros (Histograma i t xs) = zipWith4 Casillero listaMinimo listaMaximo  xs  listaPorcentaje
  where 
    intervalo   = [i + (fromIntegral j)*t | j <- [0..(length xs - 2)]]
    listaMinimo = [infinitoNegativo] ++ intervalo
    listaMaximo = intervalo ++ [infinitoPositivo]
    listaPorcentaje = if sumatoria == 0 then replicate n 0  else map (\c -> 100*fromIntegral c / fromIntegral sumatoria) xs
    sumatoria = sum xs
    n =  length xs
