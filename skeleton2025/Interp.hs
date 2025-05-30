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

-- Interpretaciones de los constructores de Dibujo

--interpreta el operador de rotacion
interp_rotar :: ImagenFlotante -> ImagenFlotante
--interp_rotar p a b c = p (a V.+ b) c (V.negate b)
interp_rotar p a b c =  p (a V.+ b) c (mulSV (-1) b)

--interpreta el operador de espejar
interp_espejar :: ImagenFlotante -> ImagenFlotante
interp_espejar p a b c = p (a V.+ b) (mulSV (-1) b) c

--interpreta el operador de rotacion 45
interp_rotar45 :: ImagenFlotante -> ImagenFlotante
interp_rotar45 p a b c = p (a V.+ mitad(b V.+ c)) (mitad(b V.+ c)) (mitad(c V.- b))

-- interp_apilar y interp_juntar  siguen la misma logica; dividir el espacio en partes proporcionales
--voy a definir una funcion auxiliar para dividir el espacio en partes proporcionales, asi queda mas conciso
dividirEspacio :: Float -> Float -> Vector -> (Vector, Vector)
dividirEspacio m n h =
  let total =  (n + m)
      r = (m / total) V.* h
      rprima = (n / total) V.* h
  in (r, rprima)

--interpreta el operador de apilar
interp_apilar :: Float -> Float -> ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_apilar m n p1 p2 d w h =
  let (r, rprima) = dividirEspacio m n h
  in Pictures [ p1 (d V.+ rprima) w r, p2 d w rprima]

--interpreta el operador de juntar
interp_juntar :: Float -> Float -> ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_juntar n m p1 p2 d w h =
  let (r, rprima) = dividirEspacio n m w
  in Pictures [p1 d r h, p2 (d V.+ r) rprima h]

--interpreta el operador de encimar 
interp_encimar :: ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_encimar p1 p2 a b c = Pictures [p1 a b c, p2 a b c]

--interpreta cualquier expresion del tipo Dibujo a
--utilizar foldDib
interp :: Interpretacion a -> Dibujo a -> ImagenFlotante
interp p = foldDib p interp_rotar interp_rotar45 interp_espejar interp_apilar interp_juntar interp_encimar