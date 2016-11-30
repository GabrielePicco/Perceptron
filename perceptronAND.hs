import Perceptron

{-|
  TEST CON FUNZIONE AND
-}

-- Converte gli input da 0,1 a -1,1
setBinInputs :: (Num a, Eq a, Num b) => [a] -> [b]
setBinInputs xs = [if x == 0 then (-1) else 1 | x <- xs]

-- Converte gli output da -1,1 a 0,1
setOutputsFromBin :: (Num a, Eq a, Num b) => [a] -> [b]
setOutputsFromBin xs = [if x == -1 then 0 else 1 | x <- xs]

-- Carica i dati ed effettua il fit del Perceptron
testCaseANDLoad :: Perceptron Double -> Perceptron Double
testCaseANDLoad ppn = fit ppn iter eta trainDataAND (setBinInputs targetAND)
   where
       iter = defaultNIter
       eta = defaultEta

-- Train Data per la funzione AND
trainDataAND =
   [[0,0]
   ,[0,1]
   ,[1,0]
   ,[1,1]]

-- Target data per la funzione AND
targetAND = [0,0,0,1]

-- Test del Perceptron per la funzione AND
testCaseAND :: IO ()
testCaseAND = do
                let ppn = testCaseANDLoad (initPerceptrons 0 [0, 0])
                putStr "\n"
                print ppn
                putStr "\nAccuratezza del Perceptron: "
                print (accuracyScore ppn trainDataAND (setBinInputs targetAND))
                putStr "\nOutput del Perceptron:\n\n"
                putStr "Test Data:"
                print trainDataAND
                putStr "Output:"
                print (setOutputsFromBin (predictMultiple ppn trainDataAND))
                return ()
