module Perceptron
( Perceptron
, initPerceptrons
, fit
, predict
, predictMultiple
, accuracyScore
, defaultNIter
, defaultEta
) where

-- Numero default di iterazioni sul dataset
defaultNIter :: Int
defaultNIter = 10

-- Learning rate default
defaultEta :: Double
defaultEta = 0.1

data Perceptron a = Perceptron {
    threshold   :: a,
    weights     :: [a]  -- lista dei pesi
} deriving Show

{-|
  Parameters
  -----------
   a : threshold
  [a]: lista pesi

  Returns
  --------
  Perceptron a
-}
initPerceptrons :: Fractional a => a -> [a] -> Perceptron a
initPerceptrons t w =
    Perceptron {
        threshold = t,
        weights = w
    }

{-|
  Parameters
  -----------
   a : threshold
  [a]: lista pesi

  Returns
  --------
  Perceptron a
-}
fit :: (Fractional a, Ord a) => Perceptron a -> Int -> a -> [[a]] -> [a] -> Perceptron a
fit ppn _ _ [] []  = ppn
fit ppn iter eta xs ts
        | iter == 0 = ppn
        | otherwise = fit (fitOnce ppn eta xs ts) (iter - 1) eta xs ts

{-|
  Parameters
  -----------
  Perceptron a : Perceptron
  a     : eta, learning rate
  [[a]] : training data
  [a]   : target data
  Returns
  --------
  Perceptron a : il Perceptron aggiornato
-}
fitOnce :: (Fractional a, Ord a) => Perceptron a -> a -> [[a]] -> [a] -> Perceptron a
fitOnce ppn _ [] []  = ppn
fitOnce ppn eta (xs:xss) (t:ts) = fitOnce (updatePerceptron ppn xs t eta) eta xss ts

{-|
  Parameters
  -----------
  Perceptron a : Perceptron
  [a] : training set
   a  : target
   a  : eta, learning rate
  Returns
  --------
  Perceptron a : il Perceptron aggiornato
      threshold = threshold + update
      weights = weights + (xs*update)
        dove update = eta*(target - predictOutput)
-}
updatePerceptron :: (Fractional a, Ord a) => Perceptron a -> [a] -> a -> a -> Perceptron a
updatePerceptron (ppn@(Perceptron { threshold = t, weights = w })) xs target eta =
        Perceptron {
            threshold = t + update,
            weights = sumList w (multList update xs)
        }
        where update = eta*(target - predict ppn xs)

{-|
  Parameters
  -----------
  Perceptron a : Perceptron
  [a] : Test set
  Returns
  --------
  a : valore (Non binario) predetto
-}
netInput :: Fractional a => Perceptron a -> [a] -> a
netInput (Perceptron { threshold = t, weights = w }) xs = (dotProduct xs w) + t

{-|
  Parameters
  -----------
  Perceptron a : Perceptron
  [a] : Test set
  Returns
  --------
  b : valore binario predetto
-}
predict :: (Fractional a, Ord a, Num b) => Perceptron a -> [a] -> b
predict ppn xs | netInput ppn xs >= 0 = 1
               | otherwise            = -1

{-|
 Parameters
 -----------
 Perceptron a : Perceptron
 [[a]] : Test data
 Returns
 --------
 [b] : lista di valori binari predetti per Test data
-}
predictMultiple :: (Fractional a, Ord a, Num b) => Perceptron a -> [[a]] -> [b]
predictMultiple ppn = map (predict ppn)

{-|
 Parameters
 -----------
 Perceptron a : Perceptron
 [a] : Lista 1
 [a] : Lista 2
 Returns
 --------
 b : accuratezza del Perceptron
-}
accuracyScore :: (Fractional a, Ord a, Fractional b) => Perceptron a -> [[a]] -> [a] -> b
accuracyScore ppn xss ys | length xs /= length ys = -1
                         | otherwise              = countEguals xs ys / fromIntegral (length ys)
                                where xs = predictMultiple ppn xss

{-|
  Parameters
  -----------
  [a]: lista 1
  [a]: lista 2

  Returns
  --------
  a: prodotto scalare dei vettori
-}
dotProduct :: Fractional a => [a] -> [a] -> a
dotProduct [] _ = 0
dotProduct _ [] = 0
dotProduct (x:xs) (y:ys) = x * y + dotProduct xs ys

{-|
  Parameters
  -----------
  [a]: lista 1
  [a]: lista 2

  Returns
  --------
  [a]: lista1 + lista2
-}
sumList :: Fractional a => [a] -> [a] -> [a]
sumList = zipWith (+)

{-|
  Parameters
  -----------
   a : num da moltiplicare nella lista
  [a]: lista

  Returns
  --------
  [a]: prodotto [a]*a
-}
multList :: Fractional a => a -> [a] -> [a]
multList = map . (*)

{-|
  Parameters
  -----------
  [a]: lista 1
  [a]: lista 2

  Returns
  --------
  b: Numero di elementi ugali nella stessa posizione tra Lista 1 e Lista 2
-}
countEguals :: (Eq a, Num b) => [a] -> [a] -> b
countEguals [] _                      = 0
countEguals _ []                      = 0
countEguals (x:xs) (y:ys) | x == y    = 1 + countEguals xs ys
                          | otherwise = countEguals xs ys
