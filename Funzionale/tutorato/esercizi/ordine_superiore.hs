-- Senza usare le ricorsioni, calcolare la somma degli elementi di una lista di numeri.
listSum :: Num a => [a] -> a
listSum x = foldl (+) 0 x

-- Senza usare le ricorsioni, calcolare la media (intera) degli elementi pari di una lista di numeri interi.
media :: [Int] -> Int
media x = div (listSum x) (length x) 

-- Senza usare le ricorsioni, calcolare il prodotto degli elementi non nulli di una lista di numeri.
prod :: (Eq a, Num a) => [a] -> a
prod xs = foldl (*) 1 (filter ((/=) 0) xs)

-- Senza usare le ricorsioni, calcolare il modulo di un vettore rappresentato come lista di numeri.
v_module :: Floating a => [a] -> a
v_module x = sqrt (foldl (+) 0 (map (^2) x))

-- Senza usare le ricorsioni, verificare se una lista è ordinata.
isSorted :: (Ord a) => [a] -> Bool
isSorted xs = and (zipWith (<=) xs (drop 1 xs)) -- and è una funzione che restituisce True se tutti gli elementi della lista sono True.

-- Senza usare le ricorsioni, concatenare due liste (senza usare l’operatore di concatenazione).
append :: [a] -> [a] -> [a]
append xs ys = foldr (:) ys xs

-- Senza usare le ricorsioni, concatenare tutte le liste di una lista di liste.
concatList :: [[a]] -> [a]
concatList xss = foldr (\xs list -> foldr (:) list xs) [] xss
-- usando list comprehension
concatComp :: [[a]] -> [a]
concatComp xss = [x | xs <- xss, x <- xs]
-- usando foldr e flip
concatPF :: [[a]] -> [a]
concatPF = foldr (flip (foldr (:))) []