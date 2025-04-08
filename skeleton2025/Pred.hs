module Pred where

import Dibujo

type Pred a = a -> Bool

--Para la definiciones de la funciones de este modulo, no pueden utilizar
--pattern-matching, sino alto orden a traves de la funcion foldDib, mapDib 

-- Dado un predicado sobre básicas, cambiar todas las que satisfacen
-- el predicado por el resultado de llamar a la función indicada por el
-- segundo argumento con dicha figura.
-- Por ejemplo, `cambiar (== Triangulo) (\x -> Rotar (Basica x))` rota
-- todos los triángulos.
cambiar :: Pred a -> (a -> Dibujo a) -> Dibujo a -> Dibujo a
cambiar pred f = mapDib2 (\x -> if pred x then f x else Basica x)

-- Alguna básica satisface el predicado.
anyDib :: Pred a -> Dibujo a -> Bool
anyDib p = foldDib fBasica fRotar fRotar45 fEspejar fApilar fJuntar fEncima
 where
    fBasica a = p a
    fRotar = id
    fRotar45 = id
    fEspejar = id
    fApilar _ _ a1 a2 = a1 || a2
    fJuntar _ _ a1 a2 = a1 || a2
    fEncima a1 a2 = a1 || a2


-- Todas las básicas satisfacen el predicado.
allDib :: Pred a -> Dibujo a -> Bool
allDib p = foldDib fBasica fRotar fRotar45 fEspejar fApilar fJuntar fEncima
 where
    fBasica a = p a
    fRotar = id
    fRotar45 = id
    fEspejar = id
    fApilar _ _ a1 a2 = a1 && a2
    fJuntar _ _ a1 a2 = a1 && a2
    fEncima a1 a2 = a1 && a2

-- Predicado auxiliar para verificar 4 rotaciones consecutivas
esCuatroRotaciones :: Dibujo a -> Bool
esCuatroRotaciones (Rotar (Rotar (Rotar (Rotar _)))) = True
esCuatroRotaciones _ = False

-- Hay 4 rotaciones seguidas.
esRot360 :: Pred (Dibujo a)
esRot360 = (>= 4) . foldDib (const 0) fRotar fRotar45 fEspejar fApilar fJuntar fEncima
 where
    fRotar x = x + 1
    fRotar45 x = x
    fEspejar x = x
    fApilar _ _ x1 x2 = max x1 x2
    fJuntar _ _ x1 x2 = max x1 x2
    fEncima x1 x2 = max x1 x2


-- Hay 2 espejados seguidos.
esFlip2 :: Pred (Dibujo a)
esFlip2 =  (>= 2) . foldDib (const 0) fRotar fRotar45 fEspejar fApilar fJuntar fEncima
 where
    fRotar x = x
    fRotar45 x = x
    fEspejar x = x + 1
    fApilar _ _ x1 x2 = max x1 x2
    fJuntar _ _ x1 x2 = max x1 x2
    fEncima x1 x2 = max x1 x2


data Superfluo = RotacionSuperflua | FlipSuperfluo
    deriving (Show, Eq)

---- Chequea si el dibujo tiene una rotacion superflua
errorRotacion :: Dibujo a -> [Superfluo]

errorRotacion d = snd (foldDib fBasica fRotar fRotar45 fEspejar fApilar fJuntar fEncima d) -- Solo nos interesa la lista de errores (snd), no el contador.
  where
    fBasica _ = (0, [])
    -- Cada vez que vemos un Rotar, sumamos 1 al contador.
    -- Si llegamos a 4 rotaciones seguidas, eso es un error.
    fRotar (n, errs) =
      let n' = n + 1
          nuevoErr = if n' == 4 then [RotacionSuperflua] else []
      in (n', errs ++ nuevoErr)

    -- Culaquier otro caso el contador se reinicia.
    fRotar45 _ = (0, [])
    fEspejar _ = (0, [])
    -- Cuando combinamos dos dibujos, reiniciamos el contador
    -- y juntamos los errores que vengan de cada parte.
    fApilar _ _ (_, e1) (_, e2) = (0, e1 ++ e2)
    fJuntar _ _ (_, e1) (_, e2) = (0, e1 ++ e2)
    fEncima    (_, e1) (_, e2) = (0, e1 ++ e2)


-- Chequea si el dibujo tiene un flip superfluo
errorFlip :: Dibujo a -> [Superfluo]
-- Se usa foldDib para recorrer el dibujo y analizar cada constructor del lenguaje.
-- Usamos snd para quedarnos con la lista de errores, el resultado de flodDib es (Bool,[Superfluo])
errorFlip d = snd (foldDib fBasica fRotar fRotar45 fEspejar fApilar fJuntar fEncima d)
  where
    fBasica _ = (0, [])

    -- Reinicia el contador de espejados, pero conserva errores
    fRotar (_, errs) = (0, errs)
    fRotar45 (_, errs)= (0, errs)

    fEspejar (n, errs) =
      let n' = n + 1
          nuevoErr = if n' == 2 then [FlipSuperfluo] else []    -- Si n' es el segundo "Espejar" implica FlipSuperfluo
          nFinal = if n' == 2 then 0 else n'                    -- Se reinicia el contador si ya habia dos "Espejar".
      in (nFinal, errs ++ nuevoErr)                             -- Devolvemos el nuevo contador nFinal y los errores acumulados.
    
    --Combina errores de dos ramas, reinicia contador
    fApilar _ _ (_, e1) (_, e2)= (0, e1 ++ e2)
    fJuntar _ _ (_, e1) (_, e2)= (0, e1 ++ e2)
    fEncima (_, e1) (_, e2)= (0, e1 ++ e2)

-- Aplica todos los chequeos y acumula todos los errores, y
-- sólo devuelve la figura si no hubo ningún error.
checkSuperfluo :: Dibujo a -> Either [Superfluo] (Dibujo a)

