--
--  概要
--    新潟市オープンデータのウェブページからメタデータを抽出します。
--    <http://www.city.niigata.lg.jp/shisei/seisaku/it/open-data/index.html>
--
--  使用法
--    niigata-city-meta-data OPEN-DATA.html > META.k
--
 
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import qualified Data.ByteString                as B
import qualified Data.Text                      as T
import qualified System.Environment             as Sys
import qualified System.FilePath                as Sys
import qualified Text.XmlHtml                   as X
import qualified Koshucode.Baala.Base           as K
import qualified Koshucode.Baala.Core           as K
import qualified Koshucode.Baala.Type.Vanilla   as K


-- --------------------------------------------  Main

main :: IO ()
main =
    do args <- Sys.getArgs
       mapM_ meta args

meta :: FilePath -> IO ()
meta path =
    do bs <- B.readFile path
       case X.parseHTML path bs of
         Left  msg -> error msg
         Right doc -> do K.putLines [ "-*- koshu -*-", "", about path, "" ]
                         K.putJudges $ map judge $ concatMap select $ X.docContent doc

about :: FilePath -> String
about path = "about" ++ K.writeDownTerms K.shortEmpty xs where
    xs :: [(String, K.VContent)]
    xs = [ term "data" $ K.pText $ Sys.takeBaseName path ]

judge :: MetaDatum -> K.JudgeC
judge (n, c) = K.affirm "META" [ term "name"    $ K.pText n
                               , term "content" $ pMaybeText c ]

term :: String -> c -> (String, c)
term n c = (n, c)

pMaybeText :: (K.CContent c) => String -> c
pMaybeText "" = K.empty
pMaybeText s  = K.pText s


-- --------------------------------------------  Select

type MetaDatum = (String, String)

nodeString :: X.Node -> String
nodeString = T.unpack . X.nodeText

select :: X.Node -> [MetaDatum]
select html = selectTable html ++ selectList html

--  <table>
--    <caption>メタデータ情報</caption>
--    <tr><th>/name</th><td>/content</td></tr>
--    <tr><th>/name</th><td>/content</td></tr>
--    ...
--  </table>
--
--  という HTML ツリーを、つぎのリストに変換します。
--
--  [("/name", "/content"), ("/name", "/content"), ...]

selectTable :: X.Node -> [MetaDatum]
selectTable html =
    do table    <- X.descendantElementsTag "table" html
       caption  <- X.childElementsTag "caption" table
       K.guard   $ X.nodeText caption == "メタデータ情報"
       tr       <- X.childElementsTag "tr" table
       (th, td) <- selectThTd tr
       return (nodeString th, K.trimBoth $ nodeString td)

selectThTd :: X.Node -> [(X.Node, X.Node)]
selectThTd tr = zip th td where
    th = X.childElementsTag "th" tr
    td = X.childElementsTag "td" tr

--  <ul>
--    <li>タイトル /content</li>
--    <li>/name /content</li>
--    ...
--  </ul>
--
--  という HTML ツリーを、つぎのリストに変換します。
--
--  [("タイトル", "/content"), ("/name", "/content"), ...]

selectList :: X.Node -> [MetaDatum]
selectList html =
    do ul <- X.descendantElementsTag "ul" html
       selectList2 ul

selectList2 :: X.Node -> [MetaDatum]
selectList2 ul = keepTitle assoc where
    keepTitle xs@(("タイトル", _) : _) = xs
    keepTitle _                       = []

    assoc :: [MetaDatum]
    assoc = do li <- X.childElementsTag "li" ul
               return $ pair $ words $ nodeString li

    pair :: [String] -> (String, String)
    pair (x : xs) = (x, unwords xs)
    pair _        = ("?", "?")

