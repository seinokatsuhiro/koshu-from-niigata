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

import qualified System.Console.GetOpt          as Opt
import qualified System.Environment             as Sys
import qualified Text.CSV                       as CSV
import qualified Koshucode.Baala.Base           as K
import qualified Koshucode.Baala.Core           as K
import qualified Koshucode.Baala.Type.Vanilla   as K


-- --------------------------------------------  Option

data Option
    = OptHelp
    | OptVersion
    | OptHeader String
      deriving (Show, Eq)

optHeader :: Option -> Maybe String
optHeader (OptHeader s) = Just s
optHeader (_)           = Nothing

option :: [Opt.OptDescr Option]
option =
    [ Opt.Option "" ["header"]  (Opt.ReqArg OptHeader "FILE") "Header file" ]


-- --------------------------------------------  Main

type NumRecord = (Int, CSV.Record)
type JudgeType = (K.JudgePat, [K.TermName])

main :: IO ()
main = do args <- Sys.getArgs
          case Opt.getOpt Opt.Permute option args of
            (opts, files, []) 
                -> do hs <- readHeaders opts
                      interact $ fromCSV hs $ parseJudgeType files
            (_, _, errs) -> error $ show errs

ints :: Int -> [Int]
ints n = [n ..]

parseJudgeType :: [String] -> (K.JudgePat, [K.TermName])
parseJudgeType [] = ("CSV", map show $ ints 1)
parseJudgeType (pat@('/' : _) : _) = error $ "Pattern like term name: " ++ pat
parseJudgeType (pat : ns) = (pat, map name ns) where
    name ('/' : n) = n
    name n         = error $ "Not a term: " ++ n

fromCSV :: [String] -> JudgeType -> K.Map String
fromCSV hs jt s =
    case CSV.parseCSV "<stdin>" s of
      Left a    -> error $ show a
      Right csv -> let js = map textJudge $ number csv
                   in unlines $ appendHeader hs js
    where
      textJudge = K.writeDownJudge K.shortEmpty . judge jt

      number :: [CSV.Record] -> [NumRecord]
      number = zip (ints 1) . K.omit (== [""])

judge :: JudgeType -> NumRecord -> K.JudgeC
judge (pat, ns) (num, cs) = K.affirm pat xs where
    xs = ("#", K.pInt num) : (map toTerm $ zip ns cs)
    toTerm (n, "") = (n, K.empty)
    toTerm (n, c)  = (n, K.pText c)


-- --------------------------------------------  Header

readHeaders :: [Option] -> IO [String]
readHeaders opts =
    case K.mapMaybe optHeader opts of
      []     -> return []
      files  -> mapM readFile files

appendHeader :: [String] -> K.Map [String]
appendHeader hs body = ["** -*- koshu -*-"] ++ hs' ++ [""] ++ body where
    hs'  = map K.commentLine $ concatMap ls hs
    ls h = "" : lines h

