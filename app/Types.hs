module Types where

import Graphics.Gloss

type Pattern = [Path]

{-   TYPES   -}
data State = S { drawing    :: Bool
               , showPoints :: Bool
               , patterns   :: [(String, Pattern)]
               , pattern    :: Pattern
               , recognized :: String
               , recN       :: Float
               }