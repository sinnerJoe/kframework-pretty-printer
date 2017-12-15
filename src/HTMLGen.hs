module HTMLGen(
generateHTML
)where

import           Text.XML.Light.Input
import           Text.XML.Light as X
import qualified Data.Text as T
import           Data.Maybe
import qualified Data.List as L
import           XMLFixer
type TagName = T.Text
type Attrib = (T.Text, T.Text)
data TagData = TagData T.Text T.Text [Attrib] [TagData]

readTagData :: Element -> TagData
readTagData el = TagData tagName content attribs children
  where
    tagName = T.pack $ qName $ elName el
    attribs = map attrToPair $ elAttribs el
    children = map readTagData $ elChildren el
    content = if null children then gtAndltForHTML  $ T.pack $ X.strContent  el else T.empty

gtAndltForHTML = T.replace (T.pack "--gt--") (T.pack "&gt;") . T.replace (T.pack "--lt--") (T.pack "&lt;")

attrToPair :: Attr -> Attrib
attrToPair attr = (T.pack $ qName $ attrKey attr, T.pack $ attrVal attr)


htmlPrefix filename = T.concat [ T.pack "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"UTF-8\"><meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\"><meta http-equiv=\"X-UA-Compatible\" content=\"ie=edge\"><title>", filename , T.pack "</title><style> html, body, div, span, object, iframe, h1, h2, h3, h4, h5, h6, p, blockquote, pre, abbr, address, cite, code, del, dfn, em, img, ins, kbd, q, samp, small, strong, sub, sup, var, b, i, dl, dt, dd, ol, ul, li, fieldset, form, label, legend, table, caption, tbody, tfoot, thead, tr, th, td, article, aside, canvas, details, figcaption, figure, footer, header, hgroup, menu, nav, section, summary, time, mark, audio, video {     margin:0;     padding:0;     border:0;     outline:0;     font-size:100%;     vertical-align:baseline;     background:transparent; }  body {     line-height:1; }  article,aside,details,figcaption,figure, footer,header,hgroup,menu,nav,section {     display:block; }  nav ul {     list-style:none; }  blockquote, q {     quotes:none; }  blockquote:before, blockquote:after, q:before, q:after {     content:'';     content:none; }  a {     margin:0;     padding:0;     font-size:100%;     vertical-align:baseline;     background:transparent; }  /* change colours to suit your needs */ ins {     background-color:#ff9;     color:#000;     text-decoration:none; }  /* change colours to suit your needs */ mark {     background-color:#ff9;     color:#000;     font-style:italic;     font-weight:bold; }  del {     text-decoration: line-through; }  abbr[title], dfn[title] {     border-bottom:1px dotted;     cursor:help; }  table {     border-collapse:collapse;     border-spacing:0; }  /* change border colour to suit your needs */ hr {     display:block;     height:1px;     border:0;     border-top:1px solid #cccccc;     margin:1em 0;     padding:0; }  input, select {     vertical-align:middle; }   /* style */ .container {      display: inline-block;     padding: 20px;     text-align: right;     max-width: 800px;     /* line-height: 60px; */ }  .container-content {     line-height: 60px;      border-radius: 5px 5px 0px 5px;     min-width: 60px;     min-height: 60px;     padding: 10px;     text-align: center;     display: block;     border: 2px solid black;     z-index: 1;  }  .container-label{     line-height: 20px;     bottom:1px;     padding: 2px 4px;     border-top: none;     border-radius: 0px 0px 5px 5px;     min-width: 30px;     text-align: center;     float: right;     /* padding: 5px; */     display: block;     border: 2px black solid;     border-top: none;     transform: translate(0, -2px);     z-index: 2;     border-collapse: collapse;          /* display: inline-block; */       } body{     text-align: center; }  .main {     max-width: 800px;     background: yellow;     margin:0 auto; } </style>"
  , T.pack "</head><body>"]

htmlSuffix = T.pack "</body></html>"

htmlCellPrefix color= T.concat [T.pack "\n<div class=\"container\"> \n <div  class=\"container-content\" style=\"background: " ,
                       appliedColor ,
                       T.pack "\">"]
  where appliedColor = if T.null color then T.pack "beige" else color

htmlCellSuffix color name= T.concat [T.pack "\n</div>\
          \\n<div class=\"container-label\" style=\"background:", appliedColor, T.pack "\">\
          \\n    ", name ,
          T.pack "\n </div>\
          \\n </div>"]
  where appliedColor = if T.null color then T.pack "beige" else color
generateHTML :: Element -> T.Text -> T.Text
generateHTML el filename = T.concat [htmlPrefix filename, generateHTML' $ readTagData el, htmlSuffix]
  where
    generateHTML' (TagData tagName content attribs children) = result
      where
        txt = if null children then content else T.concat $ map generateHTML' children
        color = snd $ fromMaybe (T.empty, T.empty) $ L.find (\(k,_) -> k == T.pack "color") attribs
        result = T.concat [htmlCellPrefix color, txt, htmlCellSuffix color tagName]
