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

recrExpr :: (Float->a) -> (Float->Float->a)-> (Expr->Expr->a->a->a)->(Expr->Expr->a->a->a)->(Expr->Expr->a->a->a)->(Expr->Expr->a->a->a) ->Expr->a
recrExpr fConst fRango fSuma fResta fMult fDiv t = case t of
      Const a -> fConst a
      Rango a b -> fRango a b
      Suma x y-> fSuma x y (rec x) (rec y)
      Resta x y -> fResta x y (rec x) (rec y)
      Mult x y -> fMult x y (rec x) (rec y)
      Div x y -> fDiv x y (rec x) (rec y)
      where rec= recrExpr fConst fRango fSuma fResta fMult fDiv 


foldExpr :: (Float->a) -> (Float->Float->a)-> (a->a->a)->(a->a->a)->(a->a->a)->(a->a->a) ->Expr->a
foldExpr fConst fRango fSuma fResta fMult fDiv t = case t of
      Const a -> fConst a
      Rango a b -> fRango a b
      Suma x y-> fSuma (rec x) (rec y)
      Resta x y -> fResta (rec x) (rec y)
      Mult x y -> fMult (rec x) (rec y)
      Div x y -> fDiv (rec x) (rec y)
      where rec= foldExpr fConst fRango fSuma fResta fMult fDiv

-- | Evaluar expresiones dado un generador de números aleatorios
eval :: Expr -> G Float
eval expr = foldExpr fCons fRango fSuma fResta fMult fDiv expr
  where 
    fCons x g = (x, g)
    fRango x y g = dameUno (x, y) g
    fSuma f1 f2 g1 = (sumando1 + sumando2, g3)
      where
        (sumando1, g2) = f1 g1
        (sumando2, g3) = f2 g2
    fResta f1 f2 g1 = (minuendo - sustraendo, g3)
      where
        (minuendo, g2) = f1 g1
        (sustraendo, g3) = f2 g2
    fMult f1 f2 g1 = (factor1 * factor2, g3)
      where
        (factor1, g2) = f1 g1
        (factor2, g3) = f2 g2
    fDiv f1 f2 g1 = (factor1 / factor2, g3)
      where
        (dividendo, g2) = f1 g1
        (divisior, g3) = f2 g2

-- | @armarHistograma m n f g@ arma un histograma con @m@ casilleros
-- a partir del resultado de tomar @n@ muestras de @f@ usando el generador @g@.
armarHistograma :: Int -> Int -> G Float -> G Histograma
armarHistograma m n f g = (histograma m r xs, gen)
  where 
    (xs, gen) = muestra f n g
    r = rango95 xs

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
      where fCons x= show x
            fRango x y =   show x ++"~"++ show y
            fSuma x y fx fy=  maybeParen  (perentesisSuma(x)) fx ++ " + " ++ maybeParen  ( perentesisSuma(y)) fy
            fResta x y fx fy = maybeParen (noEsConst(x)) fx ++ " - "  ++ maybeParen (noEsConst(y)) fy
            fMult x y fx fy= maybeParen (parentesisMult(x)) fx ++ " * " ++ maybeParen ( parentesisMult(y)) fy
            fDiv x y fx fy = maybeParen (noEsConst(x)) fx ++ " / " ++ maybeParen (noEsConst(y)) fy

perentesisSuma:: Expr->Bool
perentesisSuma (Suma _ _)=False
perentesisSuma (Const _)= False
perentesisSuma (Rango _ _)= False
perentesisSuma _ =True

parentesisMult:: Expr-> Bool
parentesisMult (Mult _ _)= False
parentesisMult (Const _)= False
parentesisMult (Rango _ _)= False
parentesisMult _ =True

noEsConst:: Expr -> Bool
noEsConst (Const _)= False
noEsConst _ = True

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
