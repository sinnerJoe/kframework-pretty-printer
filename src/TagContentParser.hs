module TagContentParser
    (
    parseTagContent
    ) where
import Data.Maybe
import Data.Tree
import Data.Map as M
import Data.STRef
import qualified Data.Text as T
import Control.Monad
import Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec.Token
import Control.Applicative hiding (many, (<|>))
import Text.Parsec.Char
import Data.List as L
import qualified Text.Parsec.Prim as Pr

parseTagContent :: Int -> String  -> String
parseTagContent nr str = case result of
                        Left _ -> str
                        Right res -> if length res >= length str then res else str
    where result = runParser tagParser (0,nr*2) "" str
parseKString = try parseVoidKString <|> parseNotVoidKString

parseNotVoidKString = do
   char '"'
   content <-many1 $ (eof >> return "")  <|> try (string "\\\"") <|> string "\\" <|> quotedChar
   char '"' <?> "quote at end of cell"
   return $ '\"' : concat content ++ "\""

parseVoidKString = string "\"\""

quotedChar =
       many1 (noneOf "\\\"" )

parantheses openP closeP= do
  char openP
  Pr.modifyState (incrementStable 2)
  content <-  try tagParser <|> consumeJunkInsideParantheses closeP
  Pr.modifyState (incrementStable (-2))
  char closeP <?> "parantheses: expecting " ++ [closeP]
  return $ (openP : content) ++ [closeP]

kStringInsideBrackets = do
 char ' '
 content <- parseKString
 char ' '
 return content

listItem = do
  string "ListItem "
  let addedSpace = length "ListItem "
  Pr.modifyState (incrementStable addedSpace)
  par <- parantheses '(' ')'
  Pr.modifyState (incrementStable (-addedSpace))
  return $ "ListItem " ++ par

setItem = do
  string "SetItem "
  let addedSpace = length " SetItem "
  Pr.modifyState (incrementStable addedSpace)
  par <- parantheses '(' ')'
  Pr.modifyState (incrementStable (-addedSpace))
  return $ "SetItem " ++ par




term =   try listItem <|> try setItem <|> try anyParantheses  <|> try lambdaLike <|> try parseKString  <|>  justWord

anyParantheses = parantheses '{' '}' <|> parantheses '(' ')'

lambdaLike = do
   firstWord <- justWord
   char ' '
   content <- anyJunkParantheses
   return $ firstWord ++ (' ':content)

justWord = many $ noneOf "\\\" \n({})"

consumeList = listItem `sepEndBy1` char ' ' <* Pr.modifyState unsuggest

consumeSet= setItem `sepEndBy1` char ' ' <* Pr.modifyState unsuggest

mapItem =  do
  term1 <- term
  char ' '
  string "|->"
  char ' '
  let addedSpace = 5 + (length term1)
  Pr.modifyState (applyOrIncrement addedSpace)
  term2 <- term
  Pr.modifyState $ undoOrIncrement (-addedSpace)
  return $ term1 ++ " |-> " ++ term2

consumeMap = mapItem `sepEndBy1` char ' '

tagParser = do
   (_, spacesBefore) <- getState
   notFollowedBy eof
   char ' '
   result <- (' ':) . (++" ") <$> (L.intercalate ("\n"++genSpaces (spacesBefore)) <$> (  try consumeMap
        <|>  try consumeList <|> try consumeSet  )
        <|> parseKStringPlusSpace <|> (lambdaLike <* char ' ') )

   -- Pr.modifyState (suggest $ lastGroupLen result)
   return result

thereAreMultipleLines output = '\n' `elem` output
lastGroupLen output = if thereAreMultipleLines output then length output - lastN else length output
  where
    lastN = last $ L.findIndices (=='\n') output

genSpaces nr = replicate nr ' '
-- genSpaces nr = "#"  ++ (show nr) ++ "#"

parseKStringPlusSpace = do
   content <- parseKString <* char ' '
   return $ content ++ " "


justWordWithoutAcolades = many $ noneOf ("\" \n" ++ "{}")
justWordWithoutCircles = many $ noneOf ("\" \n" ++ "()")

consumeJunkInsideParantheses ')'= unwords <$> (parseKString <|> junkParantheses '(' ')' <|> justWordWithoutCircles ) `sepEndBy1` char ' '

consumeJunkInsideParantheses '}'= unwords <$> (parseKString <|> junkParantheses '{' '}' <|> justWordWithoutAcolades ) `sepEndBy1` char ' '

anyJunkParantheses = junkParantheses '{' '}' <|> junkParantheses '(' ')'

junkParantheses openP closeP = do
  char openP <* spaces
  content <- consumeJunkInsideParantheses closeP
  spaces *> char closeP
  let surround = surroundByPar (openP, closeP)
  return $ surround content

surroundByPar (start, end) = ((start : ) . (' ' :)) . (++ [end])

suggest n (oldSugg, stable) = (oldSugg + n, stable)

applySuggested (sugg, stable) = (sugg, stable + sugg)
undoSuggested (sugg, stable) = (0, stable - sugg)
applyOrIncrement val (sugg, stable) = if sugg /= 0 then (sugg, stable + sugg) else (sugg, stable + val)
undoOrIncrement val (sugg, stable) = if sugg /= 0  then (sugg, stable - sugg) else (0 , stable + val)
unsuggest (sugg, stable) = (0, stable)
incrementStable amount (sugg, stable) = (sugg, stable + amount)
