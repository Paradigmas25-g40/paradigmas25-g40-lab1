module Interp where
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as V

import Dibujo

-- Gloss provee el tipo Vector y Picture.
type ImagenFlotante = Vector -> Vector -> Vector -> Picture
type Interpretacion a = a -> ImagenFlotante

mitad :: Vector -> Vector
mitad = (0.5 V.*)

ejemplo :: ImagenFlotante
ejemplo a b c = Polygon [a, b, c]

ejemplo2 :: ImagenFlotante
ejemplo2 a b c = Polygon [a, b, c]

-- Interpretaciones de los constructores de Dibujo

--interpreta el operador de rotacion
interp_rotar :: ImagenFlotante -> ImagenFlotante
interp_rotar p a b c =  p (a V.+ b) b (mulSV (-1) c) 

--interpreta el operador de espejar
interp_espejar :: ImagenFlotante -> ImagenFlotante
interp_espejar p a b c = p (a V.+ b) (mulSV (-1) b) c

--interpreta el operador de rotacion 45
interp_rotar45 :: ImagenFlotante -> ImagenFlotante
interp_rotar45 p a b c = p (a V.+ mitad(b V.+ c)) (mitad(b V.+ c)) (mitad(b V.- c))

-- interp_apilar y interp_juntar  siguen la misma logica; dividir el espacio en partes proporcionales
--voy a definir una funcion auxiliar para dividir el espacio en partes proporcionales, asi queda mas conciso
dividirEspacio :: Int -> Int -> Vector -> (Vector, Vector)
dividirEspacio n m v =
  let total = fromIntegral (n + m)
      v1 = (fromIntegral n / total) V.* v
      v2 = (fromIntegral m / total) V.* v
  in (v1, v2)
--interpreta el operador de apilar
interp_apilar :: Int -> Int -> ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_apilar n m p1 p2 a b c =
  let (h1, h2) = dividirEspacio n m c
  in Pictures [p1 a b h1, p2 (a V.+ h1) b h2]
--interpreta el operador de juntar
interp_juntar :: Int -> Int -> ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_juntar n m p1 p2 a b c =
  let (w1, w2) = dividirEspacio n m b
  in Pictures [p1 a w1 c, p2 (a V.+ w1) w2 c]
  
-- 2-DO agregar comentarios para que sea mas legible para el ojo humano

--interpreta el operador de encimar
interp_encimar :: ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_encimar p1 p2 a b c = Pictures [p1 a b c, p2 a b c]

--interpreta cualquier expresion del tipo Dibujo a
--utilizar foldDib
-- 2-DO @auwugusto
interp :: Interpretacion a -> Dibujo a -> ImagenFlotante

