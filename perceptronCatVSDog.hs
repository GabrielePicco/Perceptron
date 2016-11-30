import Perceptron
import Data.List

{-|
  TEST Cat Vs Dog
-}

-- Funzione ausiliaria per la zipCsv
countUntilSpace :: String -> Int
countUntilSpace xs     | null xs   = 0
countUntilSpace (x:xs) | x == ','  = 0
                       | otherwise = 1 + countUntilSpace xs

-- Trasforma una stringa in formato .csv in una lista
zipCsv :: String -> [String]
zipCsv xs | null xs   = []
          | otherwise = take c xs : zipCsv (drop (c + 1) xs)
              where c = countUntilSpace xs

-- Converte gli input da Cat, Dog a -1,1
setBinInputs :: (Num b) => [String] -> [b]
setBinInputs xs = [if x == "Dog" then 1 else (-1) | x <- xs]

-- Converte gli output da -1,1 a Cat, Dog
setOutputsFromBin :: (Num a, Eq a) => [a] -> [String]
setOutputsFromBin xs = [if x == -1 then "Cat" else "Dog" | x <- xs]

-- Rimuove da una stringa i caratteri di fine riga
purifyString :: String -> String
purifyString = takeWhile (/= '\r')

-- Restituisce la coppia (Training set,TargetData)
loadCsvData :: String -> ([[Double]], [String])
loadCsvData dataCsv = ([map (read::String->Double) (init xs) | xs <- unpackCsvData dataCsv] , [ purifyString (last xs) | xs <- unpackCsvData dataCsv])

-- Transforma un file .csv in una lista di liste
unpackCsvData :: String -> [[String]]
unpackCsvData dataCsv = [ zipCsv xs | xs <- lines dataCsv]

-- Carica i dati ed effettua il fit del Perceptron
testCatVsDogLoad :: Perceptron Double -> [[Double]] -> [Double] -> Perceptron Double
testCatVsDogLoad ppn= fit ppn iter eta
   where
       iter = defaultNIter
       eta = defaultEta

-- Test del Perceptron sul file catVsdog.csv
testCatVsDog :: IO ()
testCatVsDog = do
                dataCsv <- readFile "catVsdog.csv"
                let (trainingData,targetSData) = loadCsvData dataCsv
                let targetData = setBinInputs targetSData
                let ppn = testCatVsDogLoad (initPerceptrons 0 [0, 0, 0]) trainingData targetData
                putStr "\n"
                print ppn
                putStr "\nAccuratezza del Perceptron: "
                print (accuracyScore ppn trainingData targetData)
                putStr "\nTest Data:"
                print trainingData
                putStr "\nOutput:"
                print (setOutputsFromBin (predictMultiple ppn trainingData))
                return ()
