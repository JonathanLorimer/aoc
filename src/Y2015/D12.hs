module Y2015.D12 where
import Data.Aeson
import Data.Scientific (Scientific)
import Data.Aeson.KeyMap (elems)

sumJSONNumbers :: Value -> Scientific
sumJSONNumbers v = 
  case v of  
    Object o -> sum . fmap sumJSONNumbers $ elems o 
    Array vec -> sum . fmap sumJSONNumbers $ vec
    Number s -> s
    String _ -> 0 
    Bool _ -> 0
    Null -> 0

sumJSONNumbersIgnoreRed :: Value -> Scientific
sumJSONNumbersIgnoreRed v = 
  case v of  
    Object o -> 
      let elements = elems o
       in if String "red" `elem` elements 
             then 0 
             else sum . fmap sumJSONNumbersIgnoreRed $ elements
    Array vec -> sum . fmap sumJSONNumbersIgnoreRed $ vec
    Number s -> s
    String _ -> 0 
    Bool _ -> 0
    Null -> 0
