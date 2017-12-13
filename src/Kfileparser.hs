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
parseKFile' :: String -> (String -> String -> IO()) -> IO()
parseKFile' path parsingFunc= do
  kFileContent <- readFile path
  let xmlPart = searchForConfiguration kFileContent
  let xmlString = T.unpack $ T.concat xmlPart
  if null xmlPart then putStrLn $ "No xml found in " ++ path
  else
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
  writeFile colorConfigFP $ XO.ppElement $ makeColorXMLConf $ fromJust parsedXML

searchForConfiguration :: String -> [T.Text]
searchForConfiguration allContent = if null selectAlmostAllLines then [] else selectLines
    where
      contentLines = map T.pack $ lines allContent
      matchBegin = matched . (?=~ [re|configuration *< *[A-Za-z]+|])
      matchEnd = matched . (?=~ (either error id $ compileRegex $ "<[ ]*/[ ]*" ++ T.unpack rootTagName ++ "[ ]*>"))
      noStartGarbage = dropWhile (not . matchBegin) contentLines
      selectAlmostAllLines = takeWhile (not . matchEnd) noStartGarbage
      selectLines = selectAlmostAllLines ++ [noStartGarbage !! length selectAlmostAllLines]
      rootTagName = head noStartGarbage ?=~/ [ed| *configuration *< *${tg}([A-Za-z]+) .*///${tg}|]
