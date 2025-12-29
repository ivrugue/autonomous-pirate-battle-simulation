module HAUS.Memory where

import qualified Data.Map as Map
import HAUS.Types

-- Memory type: Stores pairs with a String as key and values of various possible types.
type Memory = Map.Map String ValorMemory

-- Note: We use MemType Type instead of just Type because Haskell does not allow using predefined types as constructors within a data.
data ValorMemory
    = MemInt Int
    | MemFloat Float
    | MemString String
    | MemBool Bool
    | MemPosition Position
    | MemSize Size
    | MemList [ValorMemory]
    deriving (Show, Eq)


-- Functions to read from memory, with a default value if the key is not found
getInt :: Memory -> String -> Int -> Int
getInt mem key def =
    case Map.lookup key mem of
        Just (MemInt i) -> i
        _               -> def

getFloat :: Memory -> String -> Float -> Float
getFloat mem key def =
    case Map.lookup key mem of
        Just (MemFloat f) -> f
        _                 -> def

getString :: Memory -> String -> String -> String
getString mem key def =
    case Map.lookup key mem of
        Just (MemString s) -> s
        _                  -> def

getBool :: Memory -> String -> Bool -> Bool
getBool mem key def =
    case Map.lookup key mem of
        Just (MemBool b) -> b
        _                -> def

getPosition :: Memory -> String -> Position -> Position
getPosition mem key def =
    case Map.lookup key mem of
        Just (MemPosition p) -> p
        _                    -> def

getSize :: Memory -> String -> Size -> Size
getSize mem key def =
    case Map.lookup key mem of
        Just (MemSize s) -> s
        _                -> def

getList :: Memory -> String -> [ValorMemory] -> [ValorMemory]
getList mem key def =
    case Map.lookup key mem of
        Just (MemList l) -> l
        _                -> def
