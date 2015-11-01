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
import qualified Koshucode.Baala.Data           as K
import qualified Koshucode.Baala.Core           as K
import qualified Koshucode.Baala.Writer         as K


-- --------------------------------------------  Main

main :: IO ()
main =
    do args <- Sys.getArgs
       mapM_ metaIO args

metaIO :: FilePath -> IO ()
metaIO path =
    do bs <- B.readFile path
       case X.parseHTML path bs of
         Left  msg -> error msg
         Right doc -> do let a = K.writeStringWith K.shortEmpty $ about path
                             html = X.docContent doc
                             meta = map judgeMeta $ concatMap select html
                             term = map judgeTerm $ concatMap selectTerm html
                         K.putLines [ "-*- koshu -*-", "", a, "" ]
                         K.putJudges $ meta ++ term

about :: FilePath -> K.AboutC
about path = K.About xs where
    xs :: [K.Term K.BaalaC]
    xs = [ K.term "data" $ K.pText $ Sys.takeBaseName path ]

judgeMeta :: MetaDatum -> K.JudgeC
judgeMeta (n, c) = K.affirm "META" xs where
    xs = [ K.term "name"    $ K.pText n
         , K.term "content" $ K.pMaybeText c ]

judgeTerm :: (String, String, String) -> K.JudgeC
judgeTerm (name, typ, note) = K.affirm "TERM" xs where
    xs = [ K.term "name" $ K.pMaybeText name
         , K.term "type" $ K.pMaybeText typ
         , K.term "note" $ K.pMaybeText note ]


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

--  <table>
--    <tr><th>項目名</th><th>型</th><th>備考</th></tr>
--    <tr><td>/name</td><td>/type</td><td>/note</td></tr>
--    ...
--  </table>
--
--  という HTML ツリーを、つぎのリストに変換します。
--
--  [("/name", "/type", "/note"), ...]

selectTerm :: X.Node -> [(String, String, String)]
selectTerm html =
    do table <- X.descendantElementsTag "table" html
       let (tr1 : trs) = X.childElementsTag "tr" table
           (th : _)   = X.childElementsTag "th" tr1
       K.guard $ X.nodeText th == "項目名"
       tr <- trs
       case X.childElementsTag "td" tr of
         [name, typ, note] -> return ( t name, t typ, t note )
         _ -> []
    where
      t = K.trimBoth . nodeString
