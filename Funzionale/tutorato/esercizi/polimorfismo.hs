-- Realizzare una funzione con tipo Eq a => [a] -> a -> Bool 
-- che verifica la presenza di un elemento in una lista.

contains :: Eq a => a -> [a] -> Bool
contains _ [] = False
contains a (x : xs) = (a == x || contains a xs)

-- Realizzare una funzione con tipo Eq a => [a] → Bool
-- che, applicata a una lista, determina se la lista contiene elementi dupplicati.

duplicates :: Eq a => [a] -> Bool
duplicates [] = False -- una lista senza elementi non può contenere duplicati
duplicates [_] = False -- una lista con un solo elemento non può contenere duplicati (a prescindere dall'elemento)
duplicates (x : xs) | contains x xs = True
                    | otherwise = duplicates xs


-- Implementare l’algoritmo di ordinamento per fusione.
-- Funzione di ordinamento principale. 
mergeSort :: (Ord a) => [a] -> [a]
mergeSort []  = []
mergeSort [x] = [x] -- casi base: la lista è già ordinata
mergeSort xs  = merge (mergeSort left) (mergeSort right) -- effettua il merge delle liste splittate
  where
    (left, right) = splitList (length xs `div` 2) xs

-- Funzione ausiliaria per dividere una lista. 
splitList :: Int -> [a] -> ([a], [a])
splitList n xs | n <= 0 = ([], xs)
splitList _ []          = ([], [])
splitList n (x:xs)      = (x : left, right)
  where
    (left, right) = splitList (n - 1) xs

-- Funzione ausiliaria per fondere due liste già ordinate.
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys