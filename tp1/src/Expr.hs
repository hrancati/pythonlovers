module Expr
  ( Expr (..),
    recrExpr,
    foldExpr,
    eval,
    armarHistograma,
    evalHistograma,
    mostrar,
  )
where

import Generador
import Histograma

-- | Expresiones aritméticas con rangos
data Expr
  = Const Float
  | Rango Float Float
  | Suma Expr Expr
  | Resta Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  deriving (Show, Eq)

recrExpr :: (Float->a) -> (Float->Float->a)-> (Expr->Expr->a->a->a)->(Expr->Expr->a->a->a)->(Expr->Expr->a->a->a)->(Expr->Expr->a->a->a) ->Expr->a    -- funciona igual que un recr pero con constructores 
recrExpr fConst fRango fSuma fResta fMult fDiv t = case t of -- nos fjamos qué constructor utiliza y le aplicamos la función
      Const a -> fConst a
      Rango a b -> fRango a b
      Suma x y-> fSuma x y (rec x) (rec y)
      Resta x y -> fResta x y (rec x) (rec y)
      Mult x y -> fMult x y (rec x) (rec y)
      Div x y -> fDiv x y (rec x) (rec y)
      where rec= recrExpr fConst fRango fSuma fResta fMult fDiv 


foldExpr :: (Float->a) -> (Float->Float->a)-> (a->a->a)->(a->a->a)->(a->a->a)->(a->a->a) ->Expr->a    -- funciona igual que un recr pero con constructores 
foldExpr fConst fRango fSuma fResta fMult fDiv t = case t of                                          -- nos fjamos qué constructor se utiliza y aplicamos la funcion
      Const a -> fConst a
      Rango a b -> fRango a b
      Suma x y-> fSuma (rec x) (rec y)
      Resta x y -> fResta (rec x) (rec y)
      Mult x y -> fMult (rec x) (rec y)
      Div x y -> fDiv (rec x) (rec y)
      where rec = foldExpr fConst fRango fSuma fResta fMult fDiv

-- | Evaluar expresiones dado un generador de números aleatorios
-- | Evalúa expresiones dado un generador de números aleatorios
eval :: Expr -> G Float
eval expr gen = foldExpr fConst fRango fSuma fResta fMult fDiv expr gen
  where 
    fConst x     = \g -> (x, g)
    fRango x y   = \g -> dameUno (x, y) g
    fSuma f1 f2  = \g -> operador (+) f1 f2 g
    fResta f1 f2 = \g -> operador (-) f1 f2 g
    fMult f1 f2  = \g -> operador (*) f1 f2 g
    fDiv f1 f2   = \g -> operador (/) f1 f2 g 

-- | Evalúa expresiones con la ayuda de una función operador aritmético (+, -, /, *)
operador ::  (Float -> Float -> Float) -> G Float -> G Float -> G Float
operador funcionOperador f1 f2 g =  (funcionOperador a b, g2)
  where
    (a, g1) = f1 g
    (b, g2) = f2 g1

-- | @armarHistograma m n f g@ arma un histograma con @m@ casilleros
-- a partir del resultado de tomar @n@ muestras de @f@ usando el generador @g@.
armarHistograma :: Int -> Int -> G Float -> G Histograma
armarHistograma m n f g = (histograma m r xs, gen)
  where                                            -- numeros reales xs
    (xs, gen) = muestra f n g  -- genera una muestra de f aplicada n veces, devolvemos sus resultados en xs y el generador resultante gen. 
    r = rango95 xs             --Con rango95 calculamos el rango r de nuestro Histograma

-- | @evalHistograma m n e g@ evalúa la expresión @e@ usando el generador @g@ @n@ veces
-- devuelve un histograma con @m@ casilleros y rango calculado con @rango95@ para abarcar el 95% de confianza de los valores.
-- @n@ debe ser mayor que 0.
evalHistograma :: Int -> Int -> Expr -> G Histograma
evalHistograma m n expr = armarHistograma m n (eval expr) 

-- Podemos armar histogramas que muestren las n evaluaciones en m casilleros.
-- >>> evalHistograma 11 10 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.005486 0.6733038 [1,0,0,0,1,3,1,2,0,0,1,1,0],<Gen>)

-- >>> evalHistograma 11 10000 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.273895 0.5878462 [239,288,522,810,1110,1389,1394,1295,1076,793,520,310,254],<Gen>)

-- | Mostrar las expresiones, pero evitando algunos paréntesis innecesarios.
-- En particular queremos evitar paréntesis en sumas y productos anidados.
mostrar :: Expr -> String
mostrar t = recrExpr fCons fRango fSuma fResta fMult fDiv t
  where 
    fCons x              = show x     
    fRango x y           = show x ++ "~" ++ show y         
    fSuma x y str1 str2  = mostrarSegunOperacion " + " x y str1 str2 
    fResta x y str1 str2 = mostrarSegunOperacion " - " x y str1 str2    
    fMult x y str1 str2  = mostrarSegunOperacion " * " x y str1 str2    
    fDiv x y str1 str2   = mostrarSegunOperacion " / " x y str1 str2       

-- | Muestra las expresiones evitando algunos paréntesis innecesarios. Recibe como argumentos el string operacion
-- (" + " , " - " ," * " ," / "), 2 expresiones y 2 string.
mostrarSegunOperacion :: String -> Expr -> Expr -> String -> String -> String
mostrarSegunOperacion str x y str1 str2 = maybeParen (parentesis str x) str1 ++ str ++ maybeParen (parentesis str y) str2
  where
    parentesis str expr | str == " + "                  = parentesisSuma expr
                        | str == " - "  || str == " / " = parentesisRestaYDiv expr
                        | str == " * "                  = parentesisMult expr

-- Se fija para qué casos debe llevar paréntesis en el caso de una Suma
parentesisSuma:: Expr->Bool
parentesisSuma (Suma _ _) = False
parentesisSuma (Const _) = False
parentesisSuma (Rango _ _) = False
parentesisSuma _ = True

-- Se fija para qué casos debe llevar paréntesis en el caso de la Mult
parentesisMult:: Expr-> Bool   
parentesisMult (Mult _ _) = False
parentesisMult (Const _) = False
parentesisMult (Rango _ _) = False
parentesisMult _ = True

-- Se fija para qué casos debe llevar paréntesis en el caso de la Rest o Div
parentesisRestaYDiv :: Expr -> Bool     
parentesisRestaYDiv (Const _) = False
parentesisRestaYDiv _ = True

data ConstructorExpr = CEConst | CERango | CESuma | CEResta | CEMult | CEDiv
  deriving (Show, Eq)

-- | Indica qué constructor fue usado para crear la expresión.
constructor :: Expr -> ConstructorExpr
constructor (Const _) = CEConst
constructor (Rango _ _) = CERango
constructor (Suma _ _) = CESuma
constructor (Resta _ _) = CEResta
constructor (Mult _ _) = CEMult
constructor (Div _ _) = CEDiv

-- | Agrega paréntesis antes y después del string si el Bool es True.
maybeParen :: Bool -> String -> String
maybeParen True s = "(" ++ s ++ ")"
maybeParen False s = s
