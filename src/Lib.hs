{-# LANGUAGE QuasiQuotes                      #-}
module Lib
    (
      someFunc
    ) where

import GHC.IO.Handle
import Text.XML.Light as L
import Data.List
import Rainbow as R
import Control.Arrow
import Data.Maybe
import qualified System.FilePath as FP
import qualified System.Directory as D
import Text.RE.TDFA.String
import Text.RE.Replace
import TagContentParser
import XMLFixer
import Control.Monad
import System.RunCommand
someFunc :: [String] -> IO ()
someFunc args = do
    response <- runCrossPlatformCommand "krun" args
    let (kOutput, kXml) = separateOutput response
    unless (null kOutput) $ putStrLn $ kOutput
    let xmlParseResult = L.parseXMLDoc $ fixXmlString kXml
    if isNothing xmlParseResult then
      putStrLn response
    else do
      let parsedXml = fromJust xmlParseResult
      mbConfigFilePath <- findConfigFile
      if isJust mbConfigFilePath then do
        configFile <- readFile $ fromJust mbConfigFilePath
        -- let colorMap = readConfig $ fromJust $ L.parseXMLDoc configFile
        let parsedConfig =  L.parseXMLDoc configFile
        if isNothing parsedConfig then do
            mapM_ R.putChunkLn $ elementToString parsedXml
            putStrLn "Malformed colors.conf"
        else
            let colorMap =  readConfig $ fromJust parsedConfig in
            mapM_ R.putChunkLn $ elementToColouredString' parsedXml 0 colorMap
      else
        mapM_ R.putChunkLn $ elementToString parsedXml


elementToString :: Element -> [Chunk String]
elementToString el = elementToString' el 0

elementToString' :: Element -> Int -> [Chunk String]
elementToString' element depth
      | null childrenWithNames = [ prefix , R.chunk $ contSpace ++ (parseTagContent (depth+1) $ unfixXmlString $ L.strContent element) , suffix ]
      | otherwise = prefix : concatMap (`elementToString'` (depth+1)) childrenWithNames ++ [suffix]
        where
           childrenWithNames = filter (\x -> matched ((L.qName $  L.elName x) ?=~ [re|[a-zA-Z]+|])) children
           contSpace = tail $ space (depth+1)
           children = L.elChildren element
           space dp= replicate (dp*2) ' '
           openTag = ( '<' : name ) ++ ">"
           closeTag = ( '<' : '/' : name ) ++ ">"
           prefix = colorTag $ space depth ++ openTag
           suffix = colorTag $ space depth ++ closeTag
           name = qName $ elName element
           colorTag tag = R.bold $ R.chunk tag & R.fore ( green <> green)

elementToColouredString' :: Element -> Int  -> [(String, R.Radiant)] -> [Chunk String]
elementToColouredString' element depth tagColorMap
      | null children = [toChunk prefix & R.bold, toChunk $ contSpace ++ ((parseTagContent (depth+1)) $ unfixXmlString $ L.strContent element) ,toChunk suffix & R.bold ]
      | otherwise = R.bold ( toChunk prefix) : concatMap (`recursion` (depth+1)) children ++ [toChunk suffix & R.bold]
        where
           recursion a b = elementToColouredString' a b tagColorMap
           name = qName $ elName element
           children = L.elChildren element
           space dp= replicate (dp*2) ' '
           openTag = ( '<' : name ) ++ ">"
           closeTag = ( '<' : '/' : name ) ++ ">"
           prefix = space depth ++ openTag
           suffix = space depth ++ closeTag
           contSpace = tail $ space $ depth + 1
           toChunk str = R.chunk str & R.fore (white <> getTagColor name tagColorMap)

findConfigFile :: IO (Maybe FilePath)
findConfigFile = do
  mbDir <- findKompiledDir
  if isNothing mbDir then
    return Nothing
  else do
       let assumedPath = FP.combine (fromJust mbDir) "colors.conf"
       configExists <- D.doesFileExist assumedPath
       if configExists then
        return $ Just assumedPath
       else
        return Nothing



findKompiledDir :: IO (Maybe FilePath)
findKompiledDir = do
  dirs <- D.listDirectory "."
  return $ find isKompiledDir dirs


isKompiledDir str = matched $ str ?=~ [re|.*-kompiled|]

readConfig :: Element -> [(String, R.Radiant)]
readConfig xmlConfig = tagColorMap
        where
          children = L.elChildren xmlConfig
          getTagColor = strToColor . concat . words . L.strContent
          getElName = L.qName . L.elName
          tagColorMap = map (getElName &&& getTagColor) children

getTagColor tagStr tagColorMap
            | isNothing result = R.white
            | otherwise = snd $ fromJust result
                where
                  result = find ((tagStr ==) . fst) tagColorMap

colorMap :: [(String, R.Radiant)]
colorMap = [
      ("magenta", R.magenta),
      ("blue", R.blue),
      ("red", R.red),
      ("white", R.white),
      ("grey", R.grey),
      ("green", R.green),
      ("cyan", R.cyan),
      ("brightred", R.brightRed),
      ("brightgreen", R.brightGreen),
      ("yellow", R.yellow),
      ("orange", R.color256 146),
      ("skyblue", R.color256 152),
      ("purple", R.color256 140)
    ]

strToColor :: String -> R.Radiant
strToColor str = result
      where
        rawResult = find ((str ==) . fst) colorMap
        result = snd $ fromMaybe ("" , R.white)  rawResult
