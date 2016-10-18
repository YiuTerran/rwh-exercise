{-# LANGUAGE TypeSynonymInstances, OverlappingInstances, FlexibleInstances #-}
module SimpleJSON where

import Control.Arrow (second)

type JSONError = String

class JSON a where
    toJValue :: a -> JValue
    fromJValue :: JValue -> Either JSONError a

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving (Eq, Ord, Show)

instance JSON JValue where
    toJValue = id
    fromJValue = Right

instance JSON Bool where
    toJValue = JBool
    fromJValue (JBool b) = Right b
    fromJValue _ = Left "not a JSON boolean"

instance JSON String where
    toJValue               = JString

    fromJValue (JString s) = Right s
    fromJValue _           = Left "not a JSON string"

doubleToJValue :: (Double -> a) -> JValue -> Either JSONError a
doubleToJValue f (JNumber v) = Right (f v)
doubleToJValue _ _ = Left "not a JSON number"

instance JSON Int where
    toJValue = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Integer where
    toJValue = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Double where
    toJValue = JNumber
    fromJValue = doubleToJValue id

listToJValues :: (JSON a) => [a] -> [JValue]
listToJValues = map toJValue

getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _           = Nothing

getInt (JNumber n) = Just (truncate n)
getInt _           = Nothing

getDouble (JNumber n) = Just n
getDouble _           = Nothing

getBool (JBool b) = Just b
getBool _         = Nothing

getObject (JObject o) = Just o
getObject _           = Nothing

getArray (JArray a) = Just a
getArray _          = Nothing

isNull v            = v == JNull