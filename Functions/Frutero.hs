module Functions.Frutero(cajon1,cajon2,diferentes) where

-- ---------------------------------------------------------------------
-- Frutero y sus cajones: simplemente tenemos 2 cajones con algunos
-- elementos en común y otros no, la función "diferentes" nos devolverá
-- los elementos que hay en cajon1 pero no en cajon2
-- ---------------------------------------------------------------------
cajon1 :: [String]
cajon2 :: [String]
cajon1 = ["manzana","platano","naranja","elixir rojo","uvas","coco","elixir azul"]
cajon2 = ["platano","uvas","manzana","naranja","coco"]

diferentes :: Eq a => [a] -> [a] -> [a]
diferentes c1 c2 = filter (\ x -> not (elem x c2)) c1