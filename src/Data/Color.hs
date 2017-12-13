module Data.Color
(
  makeColorXMLConf
) where

import qualified Rainbow as R
import qualified Text.XML.Light as X
import qualified Text.XML.Light.Output as XO
import Control.Monad.ST
import Data.Char
import qualified Data.List as L
import Data.Maybe
type Color = String

isHexa :: String -> Bool
isHexa str = isOfRightSize && areCharsRight
    where
      upstr = map toUpper str
      isOfRightSize = length str == 7
      isHexaDigit ch = ('A' <= ch && 'F' >= ch) || ('0' <= ch && '9' >= ch)
      areCharsRight = (head str == '#') && all isHexaDigit ( tail upstr)

isColorName :: String -> Bool
isColorName str = areCharsRight
    where
      upstr = map toUpper str
      areCharsRight = all (\x-> x <= 'Z' && x >= 'A') upstr

createColor :: String -> Maybe Color
createColor str
    | isHexa str = Just str
    | isColorName str = Just str
    | otherwise = Nothing


type TagName = String
type ColorPair = (TagName, Color)

makeColorXMLConf :: X.Element -> X.Element
makeColorXMLConf = rootColorNode
  where
    rootColorNode = X.unode "colors" . colorNodes
    colorNodes = map (uncurry X.unode ) . findColorTagPairs
    findColorAttr = L.find ((== "color") . fst) . map (\x -> (X.qName $ X.attrKey x, X.attrVal x)) . X.elAttribs
    nodeToColorPair node = (,) (X.qName $ X.elName node) $ snd $ fromMaybe ("","white") (findColorAttr node)
    findColorTagPairs node
      | null $ X.elChildren node= [nodeToColorPair node]
      | otherwise = nodeToColorPair node : (concatMap findColorTagPairs $ X.elChildren node)

-- xmlConfToPairs :: X.Element -> [ColorPair]
-- xmlConfToPairs
