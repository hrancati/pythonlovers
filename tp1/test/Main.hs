module Main (main) where

import App
import Expr
import Expr.Parser
import GHC.Stack (HasCallStack)
import Generador
import Histograma
import Test.HUnit
import Util
import Util (infinitoNegativo, infinitoPositivo)

main :: IO ()
main = runTestTTAndExit allTests

-- | Función auxiliar para marcar tests como pendientes a completar
completar :: (HasCallStack) => Test
completar = TestCase (assertFailure "COMPLETAR")

allTests :: Test
allTests =
  test
    [ "Ej 1 - Util.alinearDerecha" ~: testsAlinearDerecha,
      "Ej 2 - Util.actualizarElem" ~: testsActualizarElem,
      "Ej 3 - Histograma.vacio" ~: testsVacio,
      "Ej 4 - Histograma.agregar" ~: testsAgregar,
      "Ej 5 - Histograma.histograma" ~: testsHistograma,
      "Ej 6 - Histograma.casilleros" ~: testsCasilleros,
      "Ej 7 - Expr.recrExpr" ~: testsRecr,
      "Ej 7 - Expr.foldExpr" ~: testsFold,
      "Ej 8 - Expr.eval" ~: testsEval,
      "Ej 9 - Expr.armarHistograma" ~: testsArmarHistograma,
      "Ej 10 - Expr.evalHistograma" ~: testsEvalHistograma,
      "Ej 11 - Expr.mostrar" ~: testsMostrar,
      "Expr.Parser.parse" ~: testsParse,
      "App.mostrarFloat" ~: testsMostrarFloat,
      "App.mostrarHistograma" ~: testsMostrarHistograma
    ]

testsAlinearDerecha :: Test
testsAlinearDerecha =
  test
    [ alinearDerecha 6 "hola" ~?= "  hola",
      alinearDerecha 10 "incierticalc" ~?= "incierticalc",
      --Nuestros tests
      alinearDerecha 4 "increiblementeLargo" ~?= "increiblementeLargo",
      alinearDerecha (-4) "testNegativo" ~?= "testNegativo",
      alinearDerecha (-3) "" ~?= "",
      alinearDerecha 10 "" ~?= "          " --que pasa cuando no hay string
    ]

testsActualizarElem :: Test
testsActualizarElem =
  test
    [ actualizarElem 0 (+ 10) [1, 2, 3] ~?= [11, 2, 3],
      actualizarElem 1 (+ 10) [1, 2, 3] ~?= [1, 12, 3],
      --nuestros tests
      actualizarElem 6 (+ 30) [1, 2, 3, 4, 5] ~?= [1, 2, 3, 4, 5], --caso en el que el indice esta fuera del limite de la lista,
      actualizarElem 7 (+ 800) [1, 2, 3, 4, 5, 6, 7, 8] ~?= [1, 2, 3, 4, 5, 6, 7, 808],
      actualizarElem (-2) (+ 10) [1, 2, 3, 4, 5] ~?= [1, 2, 3, 4, 5], --caso en el que el indice negativo
      actualizarElem 4 (+ infinitoPositivo) [11, -678, 544, 0, -1234, 3] ~?= [11, -678, 544, 0, infinitoPositivo, 3] --caso en el que sumamos infinitoPositivo a una posición
    ]

testsVacio :: Test
testsVacio =
  test
    [ casilleros (vacio 1 (0, 10))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 10 0 0,
              Casillero 10 infinitoPositivo 0 0
            ],
      casilleros (vacio 3 (0, 6))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 2 0 0,
              Casillero 2 4 0 0,
              Casillero 4 6 0 0,
              Casillero 6 infinitoPositivo 0 0
            ],
      --Nuestros tests
      casilleros (vacio 5 (10, 20)) --test donde el intervalo no empieza en 0
        ~?= [ Casillero infinitoNegativo 10 0 0,
              Casillero 10 12 0 0,
              Casillero 12 14 0 0,
              Casillero 14 16 0 0,
              Casillero 16 18 0 0,
              Casillero 18 20 0 0,
              Casillero 20 infinitoPositivo 0 0
            ],
      casilleros (vacio 1 (0, 1)) --un test donde solo hay 3 casilleros
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 1.0 0 0.0,
              Casillero 1.0 infinitoPositivo 0 0.0
            ],
      casilleros (vacio 5 (-91, 12)) --Un test donde el limite inferior es negativo y el superior es positivo
      ~?= [ Casillero infinitoNegativo (-91.0) 0 0.0,
            Casillero (-91.0) (-70.4) 0 0.0,
            Casillero (-70.4) (-49.8) 0 0.0,
            Casillero (-49.8) (-29.199997) 0 0.0,
            Casillero (-29.199997) (-8.599998) 0 0.0,
            Casillero (-8.599998) 12.0 0 0.0,
            Casillero 12.0 infinitoPositivo 0 0.0
          ],
      casilleros (vacio 10 (-1000, -100)) --Un test donde ambos límites son negativos
      ~?= [ Casillero infinitoNegativo (-1000.0) 0 0.0,
            Casillero (-1000.0) (-910.0) 0 0.0,
            Casillero (-910.0) (-820.0) 0 0.0,
            Casillero (-820.0) (-730.0) 0 0.0,
            Casillero (-730.0) (-640.0) 0 0.0,
            Casillero (-640.0) (-550.0) 0 0.0,
            Casillero (-550.0) (-460.0) 0 0.0,
            Casillero (-460.0) (-370.0) 0 0.0,
            Casillero (-370.0) (-280.0) 0 0.0,
            Casillero (-280.0) (-190.0) 0 0.0,
            Casillero (-190.0) (-100.0) 0 0.0,
            Casillero (-100.0) infinitoPositivo 0 0.0
          ]
    ]

testsAgregar :: Test
testsAgregar =
  let h0 = vacio 3 (0, 6)
      h1 = vacio 5 (0, 10) --agregado por nosotros
      h2 = vacio 8 (2, 7) --agregado por nosotros
   in test
        [ casilleros (agregar 0 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 1 100, -- El 100% de los valores están acá
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar 2 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 0 0,
                  Casillero 2 4 1 100, -- El 100% de los valores están acá
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar (-1) h0)
            ~?= [ Casillero infinitoNegativo 0 1 100, -- El 100% de los valores están acá
                  Casillero 0 2 0 0,
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          --Nuestros tests
          casilleros (agregar (-3) (agregar 9 h1)) --agregar 2 numeros en 2 casilleros 
            ~?= [ Casillero infinitoNegativo 0 1 50, -- El 50% de los valores están acá 
                  Casillero 0 2 0 0,
                  Casillero 2 4 0 0, 
                  Casillero 4 6 0 0,
                  Casillero 6 8 0 0,
                  Casillero 8 10 1 50, -- El 50% de los valores están acá
                  Casillero 10 infinitoPositivo 0 0 
                ],
          casilleros (agregar infinitoPositivo (agregar infinitoNegativo (agregar 300 (agregar 3 h1)))) --agregar 4 numeros en 3 casilleros
            ~?= [ Casillero infinitoNegativo 0 1 25, -- El 25% de los valores están acá 
                  Casillero 0 2 0 0,
                  Casillero 2 4 1 25, -- El 25% de los valores están acá
                  Casillero 4 6 0 0,
                  Casillero 6 8 0 0,
                  Casillero 8 10 0 0, 
                  Casillero 10 infinitoPositivo 2 50 -- El 50% de los valores están acá 
                ],
          casilleros (agregar 2.3 (agregar 2.5 (agregar 6.5 h2))) --agregar 3 numeros en 2 casilleros
            ~?= [ Casillero infinitoNegativo 2.0 0 0.0,
                  Casillero 2.0 2.625 2 66.666664, --el 66% esta aca
                  Casillero 2.625 3.25 0 0.0,
                  Casillero 3.25 3.875 0 0.0,
                  Casillero 3.875 4.5 0 0.0,
                  Casillero 4.5 5.125 0 0.0,
                  Casillero 5.125 5.75 0 0.0,
                  Casillero 5.75 6.375 0 0.0,
                  Casillero 6.375 7.0 1 33.333332, --el 33% esta aca
                  Casillero 7.0 infinitoPositivo 0 0.0
                ]
        ]

testsHistograma :: Test
testsHistograma =
  test
    [ histograma 4 (1, 5) [1, 2, 3] ~?= agregar 3 (agregar 2 (agregar 1 (vacio 4 (1, 5)))),
      --Nuestros tests
      histograma 5 (0, 10) [21, 5, infinitoPositivo, infinitoNegativo, 9] ~?= agregar 9 (agregar infinitoNegativo (agregar infinitoPositivo (agregar 5 (agregar 21 (vacio 5 (0, 10)))))), --probamos que pasa si estan los numeros son infinitoPositivo o infinitoNegativo
      histograma 8 (2, 7) [3.1416, 4, 2.34567, 81, 6.8173, 3.3333] ~?= agregar 3.3333 (agregar 6.8173 (agregar 81 (agregar 2.34567 (agregar 4 (agregar 3.1416 (vacio 8 (2, 7))))))), --probamos que pasa con numeros reales no enteros
      histograma 6 (1, 10000) [] ~?= vacio 6 (1, 10000) --probamos que pasa si le damos una lista de numeros reales vacia
    ]

testsCasilleros :: Test
testsCasilleros =
  test
    [ casilleros (vacio 3 (0, 6))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 0 0.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      casilleros (agregar 2 (vacio 3 (0, 6)))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 1 100.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      --Nuestros test
      casilleros (agregar infinitoPositivo (vacio 5 (0, 10))) --probamos agregando infinitoPositivo
        ~?= [ Casillero infinitoNegativo 0 0 0, 
              Casillero 0 2 0 0,
              Casillero 2 4 0 0, 
              Casillero 4 6 0 0,
              Casillero 6 8 0 0,
              Casillero 8 10 0 0,
              Casillero 10 infinitoPositivo 1 100 
            ],
      casilleros (agregar 9898 (agregar 666 (agregar 30 (agregar 4 (vacio 2 (16, 1024))))))
        ~?= [ Casillero infinitoNegativo 16 1 25, 
              Casillero 16 520 1 25,
              Casillero 520 1024 1 25,
              Casillero 1024 infinitoPositivo 1 25 
            ],
      casilleros (agregar infinitoNegativo (agregar infinitoPositivo (agregar 2.9999 (agregar 3.299 (vacio 8 (2, 7)))))) --probamos casillero con numeros infinitos y reales no enteros
        ~?= [ Casillero infinitoNegativo 2.0 1 025,
              Casillero 2.0 2.625 0 0, 
              Casillero 2.625 3.25 1 25,
              Casillero 3.25 3.875 1 25,
              Casillero 3.875 4.5 0 0.0,
              Casillero 4.5 5.125 0 0.0,
              Casillero 5.125 5.75 0 0.0,
              Casillero 5.75 6.375 0 0.0,
              Casillero 6.375 7.0 0 0, 
              Casillero 7.0 infinitoPositivo 1 25
            ]
    ]

testsRecr :: Test
testsRecr =
  test
    [ recrExpr (\x -> x) (\x y -> (x+y)/2) (\e1 e2 x y -> if e1 == Const 0 then y else x+y) 
      (\e1 e2 x y -> if e2 == Const 0 then x else x-y) (\e1 e2 x y -> if e1 == Const 0 || e2 == Const 0 then 0 else x*y) 
      (\e1 e2 x y -> if e2 == Const 1 then x else x/y) (Const 8) ~?= 8.0,
      recrExpr (\x -> x) (\x y -> (x+y)/2) (\e1 e2 x y -> if e1 == Const 0 then y else x+y) 
      (\e1 e2 x y -> if e2 == Const 0 then x else x-y) (\e1 e2 x y -> if e1 == Const 0 || e2 == Const 0 then 0 else x*y) 
      (\e1 e2 x y -> if e2 == Const 1 then x else x/y) (Rango 2 7) ~?= 4.5,
      recrExpr (\x -> x) (\x y -> (x+y)/2) (\e1 e2 x y -> if e1 == Const 0 then y else x+y) (\e1 e2 x y -> x-y) 
      (\e1 e2 x y -> x*y) (\e1 e2 x y -> x/y) (Suma (Const 2) (Resta (Const 6) (Const 3))) ~?= 5,
      recrExpr (\x -> x) (\x y -> (x+y)/2) (\e1 e2 x y -> if e1 == Const 0 then y else x+y) (\e1 e2 x y -> x-y) 
      (\e1 e2 x y -> if e1 == Const 0 || e2 == Const 0 then 0 else x*y) 
      (\e1 e2 x y -> if e2 == Const 1 then x else x/y) (Mult (Const 2) (Const 3)) ~?= 6,
      recrExpr (\x -> x) (\x y -> (x+y)/2) (\e1 e2 x y -> if e1 == Const 0 then y else x+y) 
      (\e1 e2 x y -> if e2 == Const 0 then x else x-y) (\e1 e2 x y -> if e1 == Const 0 || e2 == Const 0 then 0 else x*y) 
      (\e1 e2 x y -> if e2 == Const 1 then x else x/y) (Div (Const 0) (Const 2)) ~?= 0
        ]

-- Funciones lambdas utilizadas:
-- fConst x         = x
-- fRango x y       = y
-- fSuma e1 e2 x y  = ve si el primer argumento Expr es Const 0, y si lo es devuelve el segundo argumento recursivo "y"
--                    sino devuelve "x+y"
-- fResta e1 e2 x y = ve si el segundo argumento Expr es Const 0, y si lo es devuelve el primer argumento recursivo "x"
--                    sino devuelve "x-y"
-- fMult e1 e2 x y  = ve si el primer o segundo argumento Expr es Const 0, y si lo es devuelve 0 sino devuelve "x*y"
-- fDiv e1 e2 x y   = ve si el segundo argumento Expr es Const 1, y si lo es devuelve el primer argumento recursivo "x"
--                    sino devuelve "x/y"

testsFold :: Test
testsFold =
  test
    [         
      foldExpr Const Rango Suma Resta Mult Div (Const 5) ~?= (Const 5),
      foldExpr Const Rango Suma Resta Mult Div (Rango 1 2) ~?= (Rango 1 2),
      foldExpr Const Rango Suma Resta Mult Div (Suma (Const 5) (Const 5)) ~?= (Suma (Const 5) (Const 5)),
      foldExpr Const Rango Suma Resta Mult Div (Resta (Const 5) (Const 5)) ~?= (Resta (Const 5) (Const 5)),
      foldExpr Const Rango Suma Resta Mult Div (Mult (Const 4) (Const 3)) ~?= (Mult (Const 4) (Const 3)),
      foldExpr Const Rango Suma Resta Mult Div (Div (Const 10) (Const 2)) ~?= (Div (Const 10) (Const 2)),
      foldExpr (\x -> x) (\x y -> x) (\x y -> x+y) (\x y -> x-y) (\x y -> x*y) (\x y -> x/y) (Const 2) ~?= 2,
      foldExpr (\x -> x) (\x y -> (x+y)/2) (\x y -> x+y) (\x y -> x-y) (\x y -> x*y) (\x y -> x/y) (Rango 0 7) ~?= 3.5,
      foldExpr (\x -> x) (\x y -> x) (\x y -> x+y) (\x y -> x-y) (\x y -> x*y) (\x y -> x/y) 
      (Suma (Const 2) (Resta (Const 6) (Const 3))) ~?= 5,
      foldExpr (\x -> x) (\x y -> x) (\x y -> x+y) (\x y -> x-y) (\x y -> x*y) (\x y -> x/y) 
      (Mult (Const 2) (Const 3)) ~?= 6,
      foldExpr (\x -> x) (\x y -> x) (\x y -> x+y) (\x y -> x-y) (\x y -> x*y) (\x y -> x/y) 
      (Div (Const 3) (Const 2)) ~?= 1.5
    ]

-- Funciones lambdas utilizadas:
-- fConst x         = x
-- fRango x y       = devuelve el segundo argumento "y"
-- fSuma e1 e2 x y  = devuelve "x+y"
-- fResta e1 e2 x y = devuelve "x-y"
-- fMult e1 e2 x y  = devuelve "x*y"
-- fDiv e1 e2 x y   = devuelve "x/y"

testsEval :: Test
testsEval =
  test
    [ 
      fst (eval (Suma (Rango 1 5) (Const 1)) genFijo) ~?= 4.0,      
      fst (eval (Suma (Rango 1 5) (Const 1)) (genNormalConSemilla 0)) ~?= 3.7980492,
      -- el primer rango evalua a 2.7980492 y el segundo a 3.1250308
      -- fst (eval (Suma (Rango 1 5) (Rango 1 5)) (genNormalConSemilla 0)) ~?= 5.92308,
      fst (eval (Const 1) genFijo) ~?= 1.0,
      fst (eval (Suma (Const 2) (Const 3)) genFijo) ~?= 5.0,
      fst (eval (Resta (Const 4) (Const 3)) genFijo) ~?= 1.0,
      fst (eval (Mult (Const 4) (Const 3)) genFijo) ~?= 12.0,
      fst (eval (Div (Const 27) (Const 3)) genFijo) ~?= 9.0,
      fst (eval (Rango 0 30) genFijo) ~?= 15.0

    ]

testsArmarHistograma :: Test
testsArmarHistograma =
  test
    [
    casilleros (fst (armarHistograma 5 100 (eval (Suma (Rango 1 5) (Const 10))) (genNormalConSemilla 42))) ~?= 
      [
        Casillero infinitoNegativo 10.767537 4 4.0,
        Casillero 10.767537 11.618409 8 8.0,
        Casillero 11.618409 12.469282 20 20.0,
        Casillero 12.469282 13.320154 36 36.0,
        Casillero 13.320154 14.171026 20 20.0,
        Casillero 14.171026 15.021899 11 11.0,
        Casillero 15.021899 infinitoPositivo 1 1.0
      ],
    casilleros (fst (armarHistograma 4 20000 (eval (Resta (Rango 1 600) (Const 25))) (genNormalConSemilla 14)))  ~?=
      [
        Casillero infinitoNegativo (-29.510986) 509 2.545,
        Casillero (-29.510986) 121.728485 2761 13.805,
        Casillero 121.728485 272.96796 6769 33.845,
        Casillero 272.96796 424.20743 6676 33.38,
        Casillero 424.20743 575.4469 2771 13.855,
        Casillero 575.4469 infinitoPositivo 514 2.57
      ],
    casilleros (fst (armarHistograma 6 20000 (eval (Mult (Rango (-25) (-10)) (Const 4))) (genNormalConSemilla 33)))  ~?=
      [
        Casillero infinitoNegativo (-99.798134) 499 2.495,
        Casillero (-99.798134) (-89.80865) 1405 7.025,
        Casillero (-89.80865) (-79.81917) 3220 16.1,
        Casillero (-79.81917) (-69.82968) 4895 24.475,
        Casillero (-69.82968) (-59.8402) 4835 24.175,
        Casillero (-59.8402) (-49.850716) 3208 16.04,
        Casillero (-49.850716) (-39.86123) 1424 7.12,
        Casillero (-39.86123) infinitoPositivo 514 2.57
      ],
    casilleros (fst (armarHistograma 3 20 (eval (Div (Rango 8 1000) (Const 2))) (genNormalConSemilla 1)))  ~?=
      [
        Casillero infinitoNegativo (-7.452301) 0 0.0,
        Casillero (-7.452301) 174.70993 4 20.0,
        Casillero 174.70993 356.87216 10 50.0,
        Casillero 356.87216 539.0344 6 30.0,
        Casillero 539.0344 infinitoPositivo 0 0.0
      ],
    casilleros (fst (armarHistograma 4 5000 (eval (Resta (Div (Suma (Mult (Rango 10 20) (Rango 2 3)) (Rango 100 200)) (Rango 5 10)) (Const 2))) (genNormalConSemilla 55)))  ~?=
      [
        Casillero infinitoNegativo 11.721495 21 0.42,
        Casillero 11.721495 17.766083 706 14.12,
        Casillero 17.766083 23.810673 2026 40.52,
        Casillero 23.810673 29.85526 1528 30.56,
        Casillero 29.85526 35.89985 526 10.52,
        Casillero 35.89985 infinitoPositivo 193 3.86
      ] --la operacion hecha: (((10 ~ 20) * (2 ~ 3)) + (100 ~ 200)) / (5 ~ 10) - 2 
    ]

testsEvalHistograma :: Test
testsEvalHistograma =
  test
    [completar]

testsParse :: Test
testsParse =
  test
    [ parse "1" ~?= Const 1.0,
      parse "-1.7 ~ -0.5" ~?= Rango (-1.7) (-0.5),
      parse "1+2" ~?= Suma (Const 1.0) (Const 2.0),
      parse "1 + 2" ~?= Suma (Const 1.0) (Const 2.0),
      parse "1 + 2 * 3" ~?= Suma (Const 1.0) (Mult (Const 2.0) (Const 3.0)),
      parse "1 + 2 + 3" ~?= Suma (Suma (Const 1.0) (Const 2.0)) (Const 3.0),
      parse "1 + (2 + 3)" ~?= Suma (Const 1.0) (Suma (Const 2.0) (Const 3.0)),
      parse "1 + 2 ~ 3 + 4" ~?= Suma (Suma (Const 1.0) (Rango 2.0 3.0)) (Const 4.0),
      parse "1 - 2 - 3 - 4" ~?= Resta (Resta (Resta (Const 1.0) (Const 2.0)) (Const 3.0)) (Const 4.0),
      parse "(((1 - 2) - 3) - 4)" ~?= Resta (Resta (Resta (Const 1.0) (Const 2.0)) (Const 3.0)) (Const 4.0),
      parse "1 " ~?= Const 1.0,
      parse "   1    " ~?= Const 1.0
    ]

testsMostrar :: Test
testsMostrar =
  test
    [ mostrar (Div (Suma (Rango 1 5) (Mult (Const 3) (Rango 100 105))) (Const 2))
        ~?= "(1.0~5.0 + (3.0 * 100.0~105.0)) / 2.0",
      mostrar (Suma (Suma (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Suma (Const 1) (Suma (Const 2) (Suma (Const 3) (Const 4))))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Suma (Suma (Const 1) (Const 2)) (Suma (Const 3) (Const 4)))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Mult (Mult (Mult (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Mult (Const 1) (Mult (Const 2) (Mult (Const 3) (Const 4))))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Mult (Mult (Const 1) (Const 2)) (Mult (Const 3) (Const 4)))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Resta (Resta (Const 1) (Const 2)) (Resta (Const 3) (Const 4)))
        ~?= "(1.0 - 2.0) - (3.0 - 4.0)",
      mostrar (Resta (Resta (Resta (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "((1.0 - 2.0) - 3.0) - 4.0",
      mostrar (Suma (Mult (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "((1.0 + 2.0) * 3.0) + 4.0",
      mostrar (Mult (Suma (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "(1.0 + 2.0 + 3.0) * 4.0",
        mostrar (Const 1)
        ~?= "1.0",
        mostrar(Rango 6 13)
        ~?= "6.0~13.0",
        mostrar (Mult (Suma (Const 1.0) (Const 2.0)) (Const 4.0))
        ~?= "(1.0 + 2.0) * 4.0",
        mostrar (Mult (Const 2.0) (Suma (Const 1.0) (Const 2.0)))
        ~?= "2.0 * (1.0 + 2.0)",
        mostrar (Suma (Suma (Suma (Const infinitoPositivo) (Const 2)) (Const 3)) (Const 4))
        ~?= "Infinity + 2.0 + 3.0 + 4.0",
        mostrar (Div (Suma (Const 1.9) (Const 8.3)) (Mult (Const 3.7) (Const 812.93)))
        ~?= "(1.9 + 8.3) / (3.7 * 812.93)",
        mostrar (Mult ( Div(Const infinitoPositivo )(Const infinitoNegativo)) ( Resta (Const infinitoNegativo) (Const infinitoNegativo)))
        ~?= "(Infinity / -Infinity) * (-Infinity - -Infinity)",
        mostrar (Suma (Mult (Suma (Const 3) (Div (Const 0) (Const 0))) (Rango infinitoNegativo infinitoPositivo)) (Resta (Div(Const 23) (Const 0)) (Const 87)))
        ~?= "((3.0 + (0.0 / 0.0)) * -Infinity~Infinity) + ((23.0 / 0.0) - 87.0)",
        mostrar (Suma (Mult (Const 6) (Suma (Const 32) (Const 6666))) (Div (Const 26) (Rango 61 94)))
        ~?= "(6.0 * (32.0 + 6666.0)) + (26.0 / (61.0~94.0))"
    ]

testsMostrarFloat :: Test
testsMostrarFloat =
  test
    [ mostrarFloat 0.0 ~?= "0.00",
      mostrarFloat 1.0 ~?= "1.00",
      mostrarFloat (-1.0) ~?= "-1.00",
      -- Redondeo
      mostrarFloat 3.14159 ~?= "3.14",
      mostrarFloat 2.71828 ~?= "2.72",
      mostrarFloat 0.000001 ~?= "1.00e-6",
      mostrarFloat 100000 ~?= "100000.00",
      -- Infinitos
      mostrarFloat infinitoPositivo ~?= "+inf",
      mostrarFloat infinitoNegativo ~?= "-inf"
    ]

testsMostrarHistograma :: Test
testsMostrarHistograma =
  let h0 = vacio 3 (0, 6)
      h123 = agregar 1 (agregar 2 (agregar 3 h0))
   in test
        [ lines (mostrarHistograma h123)
            ~?= [ "6.00 - +inf |",
                  "4.00 - 6.00 |",
                  "2.00 - 4.00 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ 66.67%",
                  "0.00 - 2.00 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒",
                  "-inf - 0.00 |"
                ],
          lines (mostrarHistograma (agregar 1 (vacio 3 (0, 1000))))
            ~?= [ "  1000.00 - +inf |",
                  "666.67 - 1000.00 |",
                  " 333.33 - 666.67 |",
                  "   0.00 - 333.33 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ 100.00%",
                  "     -inf - 0.00 |"
                ]
        ]
