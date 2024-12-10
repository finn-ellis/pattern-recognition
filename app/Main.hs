{-
PATTERN RECOGNITION
@author Finn Ellis

Inputs:
[ Left Click ] : Draw
[ Space ]      : Show line vertices
[ Delete ]     : Clear canvas
[ S ]          : Save pattern as name
[ Enter ]      : Normalize pattern (debug/demo, do not use before saving)

-}

module Main where

import Graphics.Gloss
import Types ( State (..), Pattern )
import Input (getInput)
import Render (render)
import Recognition (recPatDist, normalizePattern, scalePattern)
import System.IO
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.FilePath ( (</>), (<.>), (-<.>) )
import Graphics.Gloss.Interface.IO.Game (playIO)
import Graphics.Gloss.Interface.Environment (getScreenSize)

-- https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
-- could also use https://hackage.haskell.org/package/split-0.2.5/docs/Data-List-Split.html#v:splitOn
-- feel like i should've written this on my own but oh well
splitOn :: (Char -> Bool) -> String -> [String]
splitOn p s =  case dropWhile p s of
    "" -> []
    s' -> w : splitOn p s''
        where (w, s'') = break p s'

decodePattern :: String -> Pattern
decodePattern = map decodePath . splitOn (=='\n')
    where
        decodePath  :: String -> Path
        decodePath  = map strToPt . splitOn (==' ')
        strToPt     :: String -> Point
        strToPt str = let digs = splitOn (==',') str
                      in (read (head digs), read (head $ tail digs))

-- lines of points "x,y" seperated by spaces
encodePattern :: Pattern -> String
encodePattern = foldl (\str path -> str ++ foldr go "" path ++ "\n") ""
    where
        go (x, y) str2 = let pfx = if str2 == "" then "" else " "
                                  in str2 ++ pfx ++ show x ++ "," ++ show y

writePattern :: FilePath -> Pattern -> IO ()
writePattern path pat = writeFile path (encodePattern $ normalizePattern pat)

update :: Float -> State -> IO State
update _ s@S{patterns=pats} = let (name, dist) = recPatDist pats (pattern s)
                              in return s { recognized = name, recN = dist }

main :: IO ()
main = do
    -- https://stackoverflow.com/questions/47159722/haskell-gloss-io-textures
    -- load patterns:
    let patDir = "./patterns"
    createDirectoryIfMissing False patDir
    patNames <- listDirectory patDir
    rawPatterns <- mapM (readFile' . (patDir </>)) patNames
    let loadedPats = zip (map (-<.> "") patNames) (map decodePattern rawPatterns)
    
    -- get save name for this pattern:
    putStrLn "Pattern Name to Load/Save: "
    saveName <- getLine
    let savePath = patDir </> saveName <.> "pat"
    
    -- load a saved path if there is one for the name
    let loadPat = maybe [] (scalePattern 500) (lookup saveName loadedPats)
    -- initialize state
    let state = S { drawing    = False
                  , showPoints = True
                  , patterns   = loadedPats
                  , pattern    = loadPat
                  , recognized = ""
                  , recN       = 0
                  }

    ss <- getScreenSize
    
    -- open the drawing ui
    playIO FullScreen white 4
        state
        (return . render ss)
        (getInput (writePattern savePath))
        update