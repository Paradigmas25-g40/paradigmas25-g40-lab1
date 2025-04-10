module Basica.BasicaDoble where
import Dibujo
import Interp
import Basica.Comun

data Basica = Triangulo | TrainguloVioleta

ejemplo :: Dibujo Basica
--ejemplo = Apilar 1 1 (Basica Triangulo) (r360 (Basica TrainguloVioleta))
--ejemplo = encimar4 (Basica TrainguloVioleta)
--ejemplo = Apilar 1 1 (Basica Triangulo) (Rotar (Basica TrainguloVioleta))
--ejemplo = Espejar (Basica TrainguloVioleta)
--ejemplo = cuarteto (Basica TrainguloVioleta) (Basica TrainguloVioleta) (Basica TrainguloVioleta) (Basica TrainguloVioleta)
--ejemplo = ciclar (Basica TrainguloVioleta)
ejemplo = Juntar 1.0 2.0 (Apilar 1.0 1.0 (Basica Triangulo) (Basica Triangulo)) (Apilar 2.0 1.0 (Basica Triangulo) (Basica Triangulo))

interpBas :: Basica -> ImagenFlotante
interpBas Triangulo = triangulo
interpBas TrainguloVioleta = trianguloVioleta
