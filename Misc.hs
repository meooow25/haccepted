import Data.Array
import Data.List

-- Miscellaneous stuff

pairs :: [a] -> [(a, a)]
pairs a = [(x, y) | (x:ys) <- tails a, y <- ys]

fArray :: Ix i => (i, i) -> (i -> a) -> Array i a
fArray b f = array b [(i, f i) | i <- range b]
