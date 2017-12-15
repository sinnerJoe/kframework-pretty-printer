{-# LANGUAGE QuasiQuotes                      #-}
import Data.Maybe
import Data.STRef
import qualified Data.Text as T
import Control.Monad
import Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec.Token
-- import Control.Applicative hiding (many, (<|>), optional)
import Text.Parsec.Char as Ch
import Data.List as L
import qualified Text.Parsec.Prim as Pr
import Data.Functor
import Text.RE.TDFA.Text
import Text.RE.Replace
import Data.String

-- import Control.Applicative
realTest = " ListItem ( \"help me\" |-> 15 \"xx\" |-> 15 15 |-> ( SetItem ( 69 ) SetItem (55) ) ) ListItem ( ( ListItem ( 15 ) ListItem ( \"hello\" ) ) |-> 15 \"key\" |-> \"value\" )"
fromRight (Right str) = str
fromRight (Left str) = "ERROR:"

main :: IO ()
main = do
  str <- readFile "nelson.k"
  -- putStrLn str
  print $ searchForConfiguration str
  -- print $ parse parseElement "Error1!" "<Kurwa> hey yo </Kurwa>"
  -- print $ parse parseNumbers "Error1!" "15 12 51"
-- searchForConfiguration :: String -> Maybe T.Text
searchForConfiguration allContent =Just $ rootTagName
      -- Just $ limitedFixXmlText $ T.unwords (selectLines)
    where
      indexBoundsKept = length noStartGarbage > length selectAlmostAllLines
      contentLines = map T.pack $ lines allContent
      matchBegin = matched . (?=~ [re| *configuration *< *[A-Za-z]+|])
      -- matchBegin = matched . (?=~ (either error id $ compileRegex " *configuration *< *[A-Za-z]+(>| )"))
      matchEnd = matched . (?=~ (either error id $ compileRegex $ "<[ ]*/[ ]*" ++ T.unpack rootTagName ++ "[ ]*>"))
      noStartGarbage = dropWhile (not . matchBegin) contentLines
      selectAlmostAllLines = takeWhile (not . matchEnd) noStartGarbage
      selectLines = selectAlmostAllLines ++ [noStartGarbage !! length selectAlmostAllLines]
      rootTagName = head noStartGarbage ?=~/ [ed| *configuration *< *${tg}([A-Za-z]+)(>| ).*///${tg}|]

insideQuotes' ('\\':'"': str) = '\\':'"':insideQuotes' str
insideQuotes' ('"': str) = '"' : limitedFixXmlString' str
insideQuotes' ('<':str) =  "--lt--" ++ insideQuotes' str
insideQuotes' (c:str) = c : insideQuotes' str


limitedFixXmlText = T.pack . limitedFixXmlString' . T.unpack

limitedFixXmlString' ('\"':str) = '\"' : insideQuotes' str
limitedFixXmlString' (c:str) = c : limitedFixXmlString' str
limitedFixXmlString' [] = []
