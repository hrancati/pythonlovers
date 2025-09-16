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
    [ -- "Ej 1 - Util.alinearDerecha" ~: testsAlinearDerecha,
      -- "Ej 2 - Util.actualizarElem" ~: testsActualizarElem,
      -- "Ej 3 - Histograma.vacio" ~: testsVacio,
      -- "Ej 4 - Histograma.agregar" ~: testsAgregar,
      -- "Ej 5 - Histograma.histograma" ~: testsHistograma,
      -- "Ej 6 - Histograma.casilleros" ~: testsCasilleros,
      -- "Ej 7 - Expr.recrExpr" ~: testsRecr,
      -- "Ej 7 - Expr.foldExpr" ~: testsFold,
      -- "Ej 8 - Expr.eval" ~: testsEval
      -- "Ej 9 - Expr.armarHistograma" ~: testsArmarHistograma,
      "Ej 10 - Expr.evalHistograma" ~: testsEvalHistograma
      -- "Ej 11 - Expr.mostrar" ~: testsMostrar,
      -- "Expr.Parser.parse" ~: testsParse,
      -- "App.mostrarFloat" ~: testsMostrarFloat,
      -- "App.mostrarHistograma" ~: testsMostrarHistograma
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
    [  
      True ~?= True
    ]

testsFold :: Test
testsFold =
  test
    [         
      foldExpr Const Rango Suma Resta Mult Div (Const 5) ~?= (Const 5),
      foldExpr Const Rango Suma Resta Mult Div (Rango 1 2) ~?= (Rango 1 2),
      foldExpr Const Rango Suma Resta Mult Div (Suma (Const 5) (Const 5)) ~?= (Suma (Const 5) (Const 5)),
      foldExpr Const Rango Suma Resta Mult Div (Resta (Const 5) (Const 5)) ~?= (Resta (Const 5) (Const 5)),
      foldExpr Const Rango Suma Resta Mult Div (Mult (Const 4) (Const 3)) ~?= (Mult (Const 4) (Const 3)),
      foldExpr Const Rango Suma Resta Mult Div (Div (Const 10) (Const 2)) ~?= (Div (Const 10) (Const 2))
    ]

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
    [completar]

testsEvalHistograma :: Test
testsEvalHistograma =
  test
    [
      casilleros (fst (evalHistograma 5 10 (Const 5) genFijo)) ~?=
        [
          Casillero infinitoNegativo 4.0 0 0.0,
          Casillero 4.0 4.4 0 0.0,
          Casillero 4.4 4.8 0 0.0,
          Casillero 4.8 5.2 10 100.0,
          Casillero 5.2 5.6 0 0.0,
          Casillero 5.6 6.0 0 0.0,
          Casillero 6.0 infinitoPositivo 0 0.0
        ],
        casilleros (fst (evalHistograma 5 10 (Rango 1 10) genFijo))  ~?=
        [
            Casillero infinitoNegativo 4.5 0 0.0,
            Casillero 4.5 4.9 0 0.0,
            Casillero 4.9 5.3 0 0.0,
            Casillero 5.3 5.7 10 100.0,
            Casillero 5.7 6.1 0 0.0,
            Casillero 6.1 6.5 0 0.0,
            Casillero 6.5 infinitoPositivo 0 0.0
        ],
        casilleros (fst (evalHistograma 5 3 (Rango 1 10) (genNormalConSemilla 0)  )) ~?=
        [
            Casillero infinitoNegativo 2.0547414 0 0.0,
            Casillero 2.0547414 4.1489725 0 0.0,
            Casillero 4.1489725 6.243204 2 66.666664,
            Casillero 6.243204 8.337435 0 0.0,
            Casillero 8.337435 10.431667 0 0.0,
            Casillero 10.431667 12.525898 1 33.333332,
            Casillero 12.525898 infinitoPositivo 0 0.0
          ]
    ]

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
