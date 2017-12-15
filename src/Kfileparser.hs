{-# LANGUAGE QuasiQuotes #-}
module Kfileparser (
parseKFile,
generateHTMLFile
) where

import           Control.Monad.ST
import           Data.Color
import           Data.List
import           Data.Maybe
import qualified Data.Text             as T
import           System.FilePath       as FP
import           Text.RE.TDFA.Text
import qualified Text.XML.Light        as X
import qualified Text.XML.Light.Output as XO
import           HTMLGen
import           XMLFixer
import           Control.Monad
parseKFile' :: String -> (String -> String -> IO()) -> IO()
parseKFile' path parsingFunc= do
  kFileContent <- readFile path
  let xmlPart = searchForConfiguration kFileContent
  if isNothing xmlPart then putStrLn $ "Everything was compiled succesfully.\n[Warning]No xml found in "
                          ++ path ++ " . You can still use pkrun with default colors."
  else
    let xmlString = T.unpack $ fromJust xmlPart in
    parsingFunc xmlString path

parseKFile str = parseKFile' str parseColors
generateHTMLFile str = parseKFile' str generateHTMLFile'

generateHTMLFile' xmlString path = do
  let parsedXml = fromJust $ X.parseXMLDoc xmlString
  let createdFolderName = init $ reverse $ dropWhile (/= '.') $ reverse path
  let htmlFilePath = FP.combine (createdFolderName ++ "-kompiled") "schema.html"
  writeFile htmlFilePath $ T.unpack $ generateHTML parsedXml $ T.pack createdFolderName
  putStrLn "HTML generated!"

parseColors xmlString path = do
  let parsedXML = X.parseXMLDoc xmlString
  let createdFolderName = init $ reverse $ dropWhile (/= '.') $ reverse path
  let colorConfigFP = FP.combine (createdFolderName ++ "-kompiled") "colors.conf"
  print colorConfigFP
  when (isJust parsedXML) $ writeFile colorConfigFP $ XO.ppElement $ makeColorXMLConf $ fromJust parsedXML


searchForConfiguration :: String -> Maybe T.Text
searchForConfiguration allContent = if not indexBoundsKept || null selectAlmostAllLines then Nothing else
      Just $ limitedFixXmlText $ T.unlines selectLines
    where
      indexBoundsKept = length noStartGarbage > length selectAlmostAllLines
      contentLines = map T.pack $ lines allContent
      matchBegin = matched . (?=~ [re|configuration *< *[A-Za-z]+|])
      matchEnd = matched . (?=~ (either error id $ compileRegex $ "<[ ]*/[ ]*" ++ T.unpack rootTagName ++ "[ ]*>"))
      noStartGarbage = dropWhile (not . matchBegin) contentLines
      selectAlmostAllLines = takeWhile (not . matchEnd) noStartGarbage
      selectLines = selectAlmostAllLines ++ [noStartGarbage !! length selectAlmostAllLines]
      rootTagName = head noStartGarbage ?=~/ [ed| *configuration *< *${tg}([A-Za-z]+) .*///${tg}|]
