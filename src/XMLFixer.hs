{-# LANGUAGE QuasiQuotes                      #-}
module XMLFixer (
  fixXmlString,
  unfixXmlString,
  separateOutput,
  limitedFixXmlText,
  unfixXmlText
) where
import           Text.RE.TDFA.String
import           Text.RE.Replace
import           Data.Maybe
import qualified Data.Text as T
fixXmlString = fixXmlLower'

fixXmlLower' ('<':c:str) = if c `notElem` '/':['A'..'Z']++['a'..'z']
                            then "--lt--" ++ fixXmlLower' (c:str)
                            else '<':c:fixXmlLower' str
fixXmlLower' ('"':str) = '"' : insideQuotes str
fixXmlLower' (c:str) = c:fixXmlLower' str
fixXmlLower' [] = []

insideQuotes ('\\':'"': str) = '\\':'"':insideQuotes str
insideQuotes ('"': str) = '"' : fixXmlLower' str
insideQuotes ('<':str) =  "--lt--" ++ insideQuotes str
insideQuotes (c:str) = c : insideQuotes str


limitedFixXmlText = T.pack . limitedFixXmlString' . T.unpack

limitedFixXmlString' ('\"':str) = '\"' : insideQuotes str
limitedFixXmlString' (c:str) = c : limitedFixXmlString' str
limitedFixXmlString' [] = []

unfixXmlText = T.replace (T.pack "--lt--") (T.pack "<")

unfixXmlString str = replaceAll "<" $ str *=~ [re|--lt--|]

separateOutput text = if isJust tagName then (takeOutput text, drop (length $ takeOutput text) text) else ([], text)
  where
    iStr = dropWhile (/='>') $ reverse text
    tagName = if null iStr || head iStr /= '>' then Nothing else Just $ reverse $ takeWhile (`elem` (['A'..'Z']++['a'..'z'])) $ tail iStr
    isTag ('<':str) = all (uncurry (==)) $ zip str $ fromJust tagName ++ ">"
    isTag _ = False
    takeOutput (c : str) = if isTag (c:str) then [] else c : takeOutput str
    takeOutput [] = []
