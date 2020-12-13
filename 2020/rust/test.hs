import qualified Data.Map as Map


-- x % a == n -> x === n (mod a)

m :: Map.Map Int Int
m = Map.fromList [ (2, 3), (3, 5), (2, 7) ] -- x % snd = fst


getM :: Map.Map Int Int -> Int
getM = (foldl1 (*)) . Map.keys

getms :: Map.Map Int Int -> Map.Map Int Int
getms map = let m1 = getM map in
                Map.map (div m1) map

