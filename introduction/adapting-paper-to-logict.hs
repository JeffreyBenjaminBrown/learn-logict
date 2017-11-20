-- from the pdf on LogicT
-- http://okmij.org/ftp/papers/LogicT.pdf

module Test where

import Control.Monad
import Control.Monad.Logic

pr :: (Foldable t, Show a) => t a -> IO ()
pr = mapM_ $ putStrLn . show

t1, t2, t3 :: MonadPlus m => m Int
t1 = mzero
t2 = return 10 `mplus` return 20 `mplus` return 30
t3 = msum (map return [10, 20, 30])

odds :: MonadPlus m => m Int
odds = return 1 `mplus` (odds >>= \a -> return $ 2 + a)

runOdds :: [Int]
runOdds = observeMany 3 odds

disjunctionFairness :: Int -- ^ `mplus` unfair (diverges); `interleave` fair
disjunctionFairness = observe $ do x <- odds `interleave` t3
                                   if even x then return x else mzero

-- | >>= unfair, >>- fair
conjunctionFairness :: Int
conjunctionFairness = let oddsPlus :: MonadLogic m => Int -> m Int
                          oddsPlus n = odds >>- \a -> return $ a + n
  in observe $ do x <- return 0 `mplus` return 1 >>- oddsPlus
                  if even x then return x else mzero

iota n = msum $ map return [1 .. n]

-- | duplicates bc it returns `n` for every pair `(n,d)` satisfying the guards
-- If you want the `n` for which no `d` satisfies `rem n d == 0`,
-- these constructs are not enough.
test_oc :: [Int]
test_oc = observeMany 20 $ do n <- odds
                              d <- iota $ n - 1
                              guard $ n > 1 && d > 1 && n `rem` d == 0
                              return n

-- | This does what the previous cannot: finds all the odd primes.
test_op = observeMany 10 $ do n <- odds
                              guard (n > 1)
                              ifte (do d <- iota $ n - 1
                                       guard $ d > 1 && n `rem` d == 0)
                                   (const mzero)
                                   (return n)
