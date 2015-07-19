--
--  DESCRIPTION
--    Convert CSV to Koshucode
--
--  USAGE
--    koshu-from-csv PAT /N /N ... < FILE.csv
--      # convert to |-- PAT /# /N /N ...
--    koshu-from-csv < FILE.csv
--      # convert to |-- CSV /# /1 /2 ...
--

{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import qualified Text.CSV                       as CSV
import qualified System.Environment             as Sys
import qualified Koshucode.Baala.Base           as K
import qualified Koshucode.Baala.Core           as K
import qualified Koshucode.Baala.Type.Vanilla   as K

type NumRecord = (Int, CSV.Record)
type JudgeType = (K.JudgePat, [K.TermName])

main :: IO ()
main = do args <- Sys.getArgs
          interact $ fromCSV $ parseJudgeType args

ints :: Int -> [Int]
ints n = [n ..]

parseJudgeType :: [String] -> (K.JudgePat, [K.TermName])
parseJudgeType [] = ("CSV", map show $ ints 1)
parseJudgeType (pat@('/' : _) : _) = error $ "Pattern like term name: " ++ pat
parseJudgeType (pat : ns) = (pat, map name ns) where
    name ('/' : n) = n
    name n         = error $ "Not a term: " ++ n

fromCSV :: JudgeType -> K.Map String
fromCSV jt s =
    case CSV.parseCSV "<stdin>" s of
      Left a    -> error $ show a
      Right csv -> let js = map textJudge $ number csv
                   in unlines $ "-*- koshu -*-" : js
    where
      textJudge = K.writeDownJudge K.shortEmpty . judge jt

      number :: [CSV.Record] -> [NumRecord]
      number = zip (ints 1) . K.omit (== [""])

judge :: JudgeType -> NumRecord -> K.JudgeC
judge (pat, ns) (num, cs) = K.affirm pat xs where
    xs = ("#", K.pInt num) : (map toTerm $ zip ns cs)
    toTerm (n, "") = (n, K.empty)
    toTerm (n, c)  = (n, K.pText c)

