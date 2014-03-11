data Wind = Strong | Light
    deriving (Show)

data Humidity = High | Low
    deriving (Show)

data Temp = Hot | Cold
    deriving (Show)

data Water = Warm | Cool
    deriving (Show)

data Variable = Wind | Humidity | Temp | Water
    deriving (Show)

type Trial = (Wind, Humidity, Temp, Water)

data Example = Positive Trial | Negative Trial
    deriving (Show)


isPositive :: Example -> Bool
isPositive examp = case examp of
        Positive _ -> True
        Negative _ -> False

lg :: (Floating a, Ord a) => a -> a
lg n
    | n <= 0 = 0
    | otherwise = logBase 2 n

getPositive :: [Example] -> [Example]
getPositive = filter isPositive 

numPositive :: [Example] -> Int
numPositive = length.getPositive

entropy :: [Example] -> Float
entropy collection = - pos_prop * lg pos_prop - neg_prop * lg neg_prop
    where
    pos_prop = (fromIntegral.numPositive) collection / (fromIntegral.length) collection
    neg_prop = 1 - pos_prop

--Gain :: [Example] -> Variable -> Float  
--Gain = undefined

--testing vars
var = (Strong , High, Hot, Warm) 
p = Positive var
n = Negative var
trainingSet = replicate 9 p ++ replicate 5 n

    



