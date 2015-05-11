--
-- Compilado de ejercicios de haskell
-- resolucion simple iterativa o recursiva
--
-- Extraído, traducido, testeado y retocado desde
-- http://www.haskell.org/haskellwiki/99_questions
--
-- por Julián Perelli
-- Catedra de Paradigmas de Programación
-- Facultad Regional La Plata
-- Universidad Tecnológica Nacional


--
-- factorial.hs
--
factorial n =
    if n == 0
        then 1
        else n * factorial (n - 1)


--
-- Último elemento de una lista
--
ultimo1 :: [a] -> a
ultimo1 [x] = x
ultimo1 (x:xs) = ultimo1 xs

ultimo2 :: [a] -> a
ultimo2 = head . reverse


--
-- Anteúltimo elemento de una lista
--
anteultimo1 :: [a] -> a
anteultimo1 = head . tail . reverse

anteultimo2 :: [a] -> a
anteultimo2 [x,y]  = x
anteultimo2 (x:xs) = anteultimo2 xs


--
-- K-esimo elemento de una lista
--
kesimo1 :: [a] -> Int -> a
kesimo1 (x:xs) 1  = x
kesimo1 [] x     = error "Indice fuera de rango"
kesimo1 (x:xs) k
  | k < 1           = error "Indice fuera de rango"
  | otherwise       = kesimo1 xs (k - 1)


--
-- Cantindad de elementos de una lista
--
-- Recursión con resolución hacia atrás
cant1 :: [a] -> Int
cant1 []        =  0
cant1 (x:xs)    =  1 + cant1 xs

-- Recursión con resolución hacia adelante
cant2 :: [a] -> Int
cant2 list = cant2_aux list 0
	where
		cant2_aux [] n = n
		cant2_aux (x:xs) n = cant2_aux xs (n + 1)

-- Operación Map + reduce sum
cant3 :: [a] -> Int
cant3 = sum . map (\x->1)


--
-- Invertir una lista
--
-- Recursión con resolución hacia atrás
reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (x:xs) = reverse1 xs ++ [x]

-- Recursión con resolución hacia adelante
reverse2 :: [a] -> [a]
reverse2 xs = reverse2_aux xs []
  where
    reverse2_aux [] xsRev     = xsRev
    reverse2_aux (x:xs) xsRev = reverse2_aux xs (x:xsRev)


--
-- Lista es palíndromo (ej: xamax)
--
-- (Eq a) es definir que el operador de igualdad se puede utilizar en
-- los elementos de la lista [a]
esPalindromo1 :: (Eq a) => [a] -> Bool
esPalindromo1 xs = xs == (reverse xs)
    where
        -- reverse copiada del anterior
        reverse :: [a] -> [a]
        reverse [] = []
        reverse (x:xs) = reverse xs ++ [x]

esPalindromo2 :: (Eq a) => [a] -> Bool
esPalindromo2 xs = p [] xs xs
   where p rev (x:xs) (y1:y2:ys) = p (x:rev) xs ys
         p rev (x:xs) [y] = rev == xs
         p rev xs [] = rev == xs


--
-- Aplanar una estructura de listas de listas
--
aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (x:xs) = x ++ aplanar xs


--
-- Eliminar duplicados consecutivos en una lista
--
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq [x] = [x]
uniq [x,y]
    | x == y    = [x]
    | otherwise = [x,y]
uniq (x1:x2:xs)
    | x1 == x2  = uniq([x2] ++ xs)
    | otherwise = [x1] ++ uniq([x2] ++ xs)


--
-- Empaquetear en sublistas los duplicados consecutivos en una lista
--
-- copie el codigo, no pude resolverlo mas facil
-- parece que podría solucionarse como el 10 (RLE).

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x:first) : pack rest
         where
           getReps [] = ([], [])
           getReps (y:ys)
                   | y == x = let (f,r) = getReps ys in (y:f, r)
                   | otherwise = ([], (y:ys))
           (first,rest) = getReps xs


--
-- Algoritmo RLE (Run Length Encoding)
-- [a,a,a,b,b,a] -> [[3,a],[2,b],[1,a]]
---- 2013 Julián Perelli - http://github.com/jperelli
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

rle []     = []
rle (x:xs) = rle_aux 1 x xs
    where
        rle_aux n x [] = [(n, x)]
        rle_aux n x (y:ys)
            | x == y    = rle_aux (n + 1) x ys
            | otherwise = (n, x) : rle_aux 1 y ys


--
-- Duplicar elementos de una lista
--
dupli1 :: [a] -> [a]
dupli1 [] = []
dupli1 (x:xs) = x:x:dupli1 xs

dupli2 :: [a] -> [a]
dupli2 [] = []
dupli2 (x:xs) = [x,x] ++ dupli2 xs


--
-- Replicar elementos de una lista una cantidad n dada
--
-- NO ANDA!

--repli :: [a] -> Int -> [a]
--repli [] n = []
--repli (x:xs) n = repetir(x n) ++ repli(xs n)
--    where
--        repetir :: a -> Int -> [a]
--        repetir x 0 = [x]
--        repetir x n = [x] ++ repetir(x (n - 1))


--
-- Eliminar un elemento cada N elementos de una lista
--
eliminarCada1 :: [a] -> Int -> [a]
eliminarCada1 xs n = auxiliar xs n n
    where auxiliar [] x y = []
          auxiliar (x:xs) n 1 = auxiliar xs n n
          auxiliar (x:xs) n k = x : (auxiliar xs n (k - 1))

-- Usando clausuras (closures):
-- El detalle esta en la llamada donde se usa n en vez de k
-- k es una variable que se encuentra en una clausura
-- n es una variable global a helper
eliminarCada2 :: [a] -> Int -> [a]
eliminarCada2 xs n = auxiliar xs n
    where auxiliar [] x = []
          auxiliar (x:xs) 1 = auxiliar xs n 
          auxiliar (x:xs) k = x : auxiliar xs (k-1)


--
-- Dividir una lista en dos
-- dada la cantidad de elementos que deberá tener la primer lista
--
dividir :: [a] -> Int -> [[a]]
dividir xs n = auxiliar [] xs n
    where
        auxiliar :: [a] -> [a] -> Int -> [[a]]
        auxiliar xs (y:ys) 1 = [xs ++ [y], ys]
        auxiliar xs (y:ys) n = auxiliar (xs ++ [y]) ys (n - 1)


--
-- Extraer una parte de una lista comprendida entre dos indices
--
extraer :: [a] -> Int -> Int -> [a]
extraer [] x y = []
extraer (x:xs) i k
 | i > 1      = extraer xs (i - 1) (k - 1)
 | k < 1      = []
 | otherwise  = x : extraer xs (i - 1) (k - 1)


--
-- Rotar una lista N lugares
--
-- Esta no compila en haskell>2010 por el (n+1)
-- rotate [] _ = []
-- rotate l 0 = l
-- rotate (x:xs) (n+1) = rotate (xs ++ [x]) n
-- rotate l n = rotate l (length l + n)

-- Reformulacion de la anterior
-- Parece andar, pero supongo que hay casos donde se rompe 
rotar1 [] x = []
rotar1 l 0 = l
rotar1 (x:xs) n = rotar1 (xs ++ [x]) (n - 1)

-- Sin usar cant()
rotar2 :: [a] -> Int -> [a]
rotar2 [] x = []
rotar2 x 0 = x
rotar2 x y
  | y > 0 = rotar2 (tail x ++ [head x]) (y-1)
  | otherwise = rotar2 (last x : init x) (y+1)


--
-- Eliminar el elemento N de una lista
--
eliminarN :: [a] -> Int -> [a]
eliminarN (x:xs) 1 = xs
eliminarN (x:xs) n = x : eliminarN xs (n - 1)


--
-- Insertar un elemento en la posicioón N de una lista
--
insertarEn :: a -> [a] -> Int -> [a]
insertarEn x ys     1 = x:ys
insertarEn x (y:ys) n = y:insertarEn x ys (n-1)


--
-- Crear una lista que contenga un rango de elementos
--
rango :: Int -> Int -> [Int]
rango n m
    | n == m = [n]
    | n < m = n:(rango (n+1) m)
    | n > m = n:(rango (n-1) m)


---- tests ----
main = do
    print "Ingresa un número: "
    x <- readLn
    print(factorial x)

    print(factorial 5)

    let x = factorial 5
    print(x)
    
    let l = [1,2,3,4]
    print(ultimo1 l)
    print(ultimo2 l)

    let l = [1,2,3,4]
    print(anteultimo1 l)
    print(anteultimo2 l)

    let l = [11,22,44,88]
    print(kesimo1 l 2)
    print(kesimo1 l 1)

    let l = [11,22,44,88]
    print(cant1 l)
    print(cant2 l)
    print(cant3 l)

    let l = [11,22,44,88]
    print(reverse1 l)
    print(reverse2 l)

    -- Los strings son listas de chars
    print(esPalindromo1 "auto")
    print(esPalindromo2 "auto")
    print(esPalindromo1 "reconocer")
    print(esPalindromo2 "reconocer")

    -- Los strings son listas de chars
    print(aplanar  [[1,2], [3], [4,5,6]])

    print(uniq  [1, 2, 3, 3, 4, 4, 5, 6, 6])           

    print(pack [1, 2, 3, 3, 3, 4, 4, 5, 6, 6])

    print(rle  [1, 2, 3, 3, 3, 4, 4, 5, 6, 6])
    print(rle  "1AAAAFF33FFFFFFFF5")

    print(dupli1  [1, 2, 3])
    print(dupli2  [1, 2, 3])

--    print(repli  [1, 2, 3] 3 )

    print(eliminarCada1  [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] 3 )
    print(eliminarCada2  [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] 3 )

    print(dividir  [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] 3 )    

    print(extraer  [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] 3 5)

    print(rotar1  [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] 3)
    print(rotar2  [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] 3)

    print(eliminarN  [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] 5)

    print(insertarEn  111 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] 5)

    print( rango 5  8 )
    print( rango 5 (-8) )
