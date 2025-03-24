module Dibujo where

-- Definir el lenguaje via constructores de tipo
data Dibujo a = Basica a
       |Rotar (Dibujo a)
       |Rotar45 (Dibujo a)
       |Espejar (Dibujo a)
       |Apilar Float Float (Dibujo a) (Dibujo a)
       |Juntar Float Float (Dibujo a) (Dibujo a)
       |Encima (Dibujo a) (Dibujo a)
       deriving(Eq,Show)

-- Composición n-veces de una función con sí misma.
comp :: (a -> a) -> Int -> a -> a
comp f 0 a = a
comp f 1 a = (f a)
comp f x a | x > 1 = comp f (x-1) a
       |x < 0 = error "No es posible componer negativamente."

-- Rotaciones de múltiplos de 90.
r180 :: Dibujo a -> Dibujo a
r180 a = comp (Rotar45) 4 a

r270 :: Dibujo a -> Dibujo a
r270 a = comp (Rotar45) 6 a

r90 :: Dibujo a -> Dibujo a
r90 a = comp (Rotar45) 2 a

-- Pone una figura sobre la otra, ambas ocupan el mismo espacio.
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) a b = Apilar 1 1 a b

-- Pone una figura al lado de la otra, ambas ocupan el mismo espacio.
(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) a b = Juntar 1 1 a b
-- Superpone una figura con otra.
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) a b = Encima a b


-- Dadas cuatro dibujos las ubica en los cuatro cuadrantes.
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto a b c d = (.-.) ((^^^) a b) ((^^^) c d)

-- Un dibujo repetido con las cuatro rotaciones, superpuestas.

encimar4 :: Dibujo a -> Dibujo a
encimar4 a = (^^^) (r270 a) $ (^^^) (r180 a) $ (^^^) (r90 a) a

-- Cuadrado con la misma figura rotada i * 90, para i ∈ {0, ..., 3}.
-- No confundir con encimar4!
ciclar :: Dibujo a -> Dibujo a
ciclar a = (.-.) ((///) a (Rotar a)) ((///) (r180 a) (r270 a))

-- Transfomar un valor de tipo a como una Basica.
pureDib :: a -> Dibujo a
pureDib a = Basica a

-- map para nuestro lenguaje.
mapDib :: (a -> b) -> Dibujo a -> Dibujo b
mapDib f (Basica a) =  Basica (f a)
mapDib f (Rotar a) = Rotar (mapDib f a)
mapDib f (Rotar45 a) = Rotar45 (mapDib f a)
mapDib f (Espejar a) = Espejar (mapDib f a)
mapDib f (Apilar x y a1 a2) = Apilar x y (mapDib f a1) (mapDib f a2)
mapDib f (Juntar x y a1 a2) = Juntar x y (mapDib f a1) (mapDib f a2)
mapDib f (Encima a1 a2) = Encima (mapDib f a1) (mapDib f a2)

-- funcion auxiliar
mapDib2 :: (a -> Dibujo b) -> Dibujo a -> Dibujo b
mapDib2 f (Basica a) =  f a
mapDib2 f (Rotar a) = Rotar (mapDib2 f a)
mapDib2 f (Rotar45 a) = Rotar45 (mapDib2 f a)
mapDib2 f (Espejar a) = Espejar (mapDib2 f a)
mapDib2 f (Apilar x y a1 a2) = Apilar x y (mapDib2 f a1) (mapDib2 f a2)
mapDib2 f (Juntar x y a1 a2) = Juntar x y (mapDib2 f a1) (mapDib2 f a2)
mapDib2 f (Encima a1 a2) = Encima (mapDib2 f a1) (mapDib2 f a2)

-- Funcion de fold para Dibujos a
foldDib ::
       (a -> b) -> 
       (b -> b) -> 
       (b -> b) -> 
       (b -> b) ->
       (Float -> Float -> b -> b -> b) -> 
       (Float -> Float -> b -> b -> b) -> 
       (b -> b -> b) ->
       Dibujo a ->
       b 
foldDib fBasica _ _ _ _ _ _  (Basica a) = fBasica a
foldDib fBasica fRotar fRotar45 fEspejar fApilar fJuntar fEncima (Rotar a) = fRotar (foldDib fBasica fRotar fRotar45 fEspejar fApilar fJuntar fEncima a)
foldDib fBasica fRotar fRotar45 fEspejar fApilar fJuntar fEncima (Rotar45 a) = fRotar45 (foldDib fBasica fRotar fRotar45 fEspejar fApilar fJuntar fEncima a)
foldDib fBasica fRotar fRotar45 fEspejar fApilar fJuntar fEncima (Espejar a) = fEspejar (foldDib fBasica fRotar fRotar45 fEspejar fApilar fJuntar fEncima a)
foldDib fBasica fRotar fRotar45 fEspejar fApilar fJuntar fEncima (Apilar x y a b) = fApilar x y (foldDib fBasica fRotar fRotar45 fEspejar fApilar fJuntar fEncima a) (foldDib fBasica fRotar fRotar45 fEspejar fApilar fJuntar fEncima b)
foldDib fBasica fRotar fRotar45 fEspejar fApilar fJuntar fEncima (Juntar x y a b) = fJuntar x y (foldDib fBasica fRotar fRotar45 fEspejar fApilar fJuntar fEncima a) (foldDib fBasica fRotar fRotar45 fEspejar fApilar fJuntar fEncima b)
foldDib fBasica fRotar fRotar45 fEspejar fApilar fJuntar fEncima (Encima a b) = fEncima (foldDib fBasica fRotar fRotar45 fEspejar fApilar fJuntar fEncima a) (foldDib fBasica fRotar fRotar45 fEspejar fApilar fJuntar fEncima b)



