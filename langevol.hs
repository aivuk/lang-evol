module LangEvol where

import qualified Data.Array as A
import qualified Data.Vector.Unboxed as V
import qualified System.Random.Mersenne as R
import Control.Monad (replicateM)

data Pop = Pop { popData :: A.Array Int (V.Vector Int)
               , popSize :: Int
               , popSymMax :: Int
               , popSymNum :: Int 
               , popRmin :: Int }
    deriving (Show)

new_pop :: R.MTGen -> Int -> Int -> Int -> Int -> IO Pop
new_pop g n_pop n_sym sym_max r_min = do
    vl <- replicateM n_pop $ return.((V.fromList).(map (`mod` sym_max)).(take n_sym)) =<< R.randoms g
    return $ Pop (A.listArray (0, n_pop - 1) vl) n_pop sym_max n_sym r_min

updatePop :: R.MTGen -> Pop -> IO Pop
updatePop g p = do
    [i,j] <- return.((map (`mod` (popSize p))).(take 2)) =<< R.randoms g
    let pi = popData p A.! i
        pj = popData p A.! j
        (num_eq, vdiff) = countEq pi pj
    if num_eq >= (popRmin p) && V.length vdiff > 0
     then do
       si <- return.(`mod` V.length vdiff) =<< R.random g
       return $ p { popData = (popData p) A.// [(j, pj V.// [(vdiff V.! si, pi V.! (vdiff V.! si))])] }
     else
       return p

updatePopN g p n = upPop p [] n
    where
        upPop p r n | n == 0 = return.reverse $ p:r
                    | otherwise = do
                                 p' <- updatePop g p
                                 upPop p' (p':r) (n - 1)

countEq :: V.Vector Int -> V.Vector Int -> (Int, V.Vector Int)
countEq v1 v2 | (V.length v1 == V.length v2) = ce 0 0 V.empty v1 v2
              | otherwise = error "Vectors of different sizes"
    where
        ce num_eq i vdiff v1 v2 | V.null v1 = (num_eq, vdiff)
                                | (V.head v1 == V.head v2) = ce (num_eq + 1) (i + 1) vdiff (V.tail v1) (V.tail v2)
                                | otherwise = ce num_eq (i + 1) (V.cons i vdiff) (V.tail v1) (V.tail v2)
