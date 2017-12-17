module TagContentParser
      (
        parseTagContent
      ) where

import           Data.Maybe
import qualified Data.Text as T
import           Text.ParserCombinators.Parsec as P
import           Text.ParserCombinators.Parsec.Token
import           Control.Applicative hiding (many, (<|>))
import           Data.List as L
import qualified Text.Parsec.Prim as Pr

parseTagContent :: Int -> String  -> String
parseTagContent nr str = case result of
                        Left _ -> str
                        Right res -> if length res >= length str then res else str
    where result = runParser tagParser (nr*2) "" str

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
  Pr.modifyState (+ 2)
  content <-  try tagParser <|> consumeJunkInsideParantheses closeP
  Pr.modifyState (+ (-2))
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
  Pr.modifyState (+ addedSpace)
  par <- parantheses '(' ')'
  Pr.modifyState (+ (-addedSpace))
  return $ "ListItem " ++ par

setItem = do
  string "SetItem "
  let addedSpace = length " SetItem "
  Pr.modifyState (+ addedSpace)
  par <- parantheses '(' ')'
  Pr.modifyState (+ (-addedSpace))
  return $ "SetItem " ++ par

term =   try listItem <|> try setItem <|> try anyParantheses  <|> try lambdaLike <|> try parseKString  <|>  justWord

anyParantheses = parantheses '{' '}' <|> parantheses '(' ')'

lambdaLike = do
   firstWord <- justWord
   char ' '
   content <- anyJunkParantheses
   return $ firstWord ++ (' ':content)

justWord = many $ noneOf "\\\" \n({})"

consumeList = listItem `sepEndBy1` char ' '

consumeSet= setItem `sepEndBy1` char ' '

mapItem =  do
  term1 <- term
  char ' '
  string "|->"
  char ' '
  let addedSpace = length term1 + 5
  Pr.modifyState (+ addedSpace)
  term2 <- term
  Pr.modifyState $ (+) (-addedSpace)
  return $ term1 ++ " |-> " ++ term2

consumeMap = mapItem `sepEndBy1` char ' '

tagParser = do
   spacesBefore <- getState
   notFollowedBy eof
   char ' '
   (' ':) . (++" ") <$> (L.intercalate ("\n"++genSpaces spacesBefore) <$> (  try consumeMap
        <|>  try consumeList <|> try consumeSet  )
        <|> parseKStringPlusSpace <|> (lambdaLike <* char ' ') )

thereAreMultipleLines output = '\n' `elem` output
lastGroupLen output = if thereAreMultipleLines output then length output - lastN else length output
  where
    lastN = last $ elemIndices '\n' output

genSpaces nr = replicate nr ' '
-- genSpaces nr = "#"  ++ (show nr) ++ "#"

parseKStringPlusSpace = parseKString <* char ' '


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
