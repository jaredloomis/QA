{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module Element where

import Data.String (IsString)

newtype Element = Element String
  deriving (Show, Eq, IsString)

elementQuery :: Element -> String
elementQuery (Element query) = escapeJSString query

escapeJSString :: String -> String
escapeJSString = concatMap
    (\case '"'  -> "\\\""
           '\'' -> "\\'"
           '\\' -> "\\\\"
           c    -> [c])
