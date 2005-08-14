{-# OPTIONS_GHC -fglasgow-exts -funbox-strict-fields -fallow-overlapping-instances -fallow-undecidable-instances #-}

module DrIFT.JSON where
import Data.Ratio
import Data.List (intersperse)
import Control.Concurrent.STM

type JSONClass = String
type JSONKey = String
type JSONVal = String

class (Show a) => JSON a where
    showJSON :: a -> String
    showJSON x = show (show x)

showJSArrayObj :: JSONClass -> [JSONVal] -> String
showJSArrayObj = showJSObj showJSArray

showJSHashObj :: JSONClass -> [(JSONKey, JSONVal)] -> String
showJSHashObj = showJSObj showJSHash

showJSArray :: [JSONVal] -> String
showJSArray xs = ('[':(concat $ intersperse "," xs)) ++ "]"

showJSHash :: [(JSONKey, JSONVal)] -> String
showJSHash xs = ('{':(concat $ intersperse "," (map showPair xs))) ++ "}"
    where
    showPair (k, v) = show k ++ (':':v)

showJSScalar :: JSONClass -> String
showJSScalar cls = ('{':show cls) ++ ":null}"

showJSObj :: (a -> String) -> JSONClass -> a -> String
showJSObj f cls dat = ('{':show cls) ++ (':':f dat) ++ "}"

-- XXX - overlapping instances?
instance JSON () where
    showJSON _ = "null"

instance JSON Int where
    showJSON = show

instance JSON String where
    showJSON = show

instance JSON Bool where
    showJSON True = "true"
    showJSON False = "false"

instance JSON Integer where 
    showJSON = show
instance JSON Rational where 
    showJSON r = showJSArrayObj "%" [show x, show y]
        where
        x = numerator r
        y = denominator r
instance JSON Double where 
    showJSON = show

instance (JSON a) => JSON (Maybe a) where
    showJSON (Just x) = showJSON x
    showJSON Nothing = "null"

instance (JSON a) => JSON [a] where
    showJSON = showJSArray . map showJSON

instance (JSON a, JSON b) => JSON (a, b) where
    showJSON (x, y) = showJSArray [showJSON x, showJSON y]

instance (JSON a, JSON b, JSON c) => JSON (a, b, c) where
    showJSON (x, y, z) = showJSArray [showJSON x, showJSON y, showJSON z]

instance (Show (TVar a)) => JSON (TVar a) where
    showJSON _ = "null"
