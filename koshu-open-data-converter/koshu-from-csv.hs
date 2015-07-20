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


-- --------------------------------------------  Param

data Param = Param
    { paramOmitFirst  :: Bool
    , paramNumber     :: Bool
    , paramHeaders    :: [String]
    , paramJudgeType  :: JudgeType
    } deriving (Show, Eq, Ord)

initParam :: [Option] -> [String] -> IO Param
initParam opts args =
    do hs <- readHeaders opts
       return $ Param { paramOmitFirst  = OptOmitFirst `elem` opts
                      , paramNumber     = OptNumber    `elem` opts
                      , paramHeaders    = hs
                      , paramJudgeType  = parseJudgeType args
                      }

readHeaders :: [Option] -> IO [String]
readHeaders opts =
    case K.mapMaybe optHeader opts of
      []     -> return []
      files  -> mapM readFile files

parseJudgeType :: [String] -> JudgeType
parseJudgeType [] = ("CSV", map show $ ints 1)
parseJudgeType (pat@('/' : _) : _) = error $ "Pattern like term name: " ++ pat
parseJudgeType (pat : ns) = (pat, map name ns) where
    name ('/' : n) = n
    name n         = error $ "Not a term: " ++ n


-- --------------------------------------------  Option

data Option
    = OptHelp
    | OptVersion
    | OptOmitFirst
    | OptNumber
    | OptHeader String
      deriving (Show, Eq, Ord)

optHeader :: Option -> Maybe String
optHeader (OptHeader s) = Just s
optHeader (_)           = Nothing

option :: [Opt.OptDescr Option]
option =
    [ Opt.Option "1" ["omit-first"] (Opt.NoArg OptOmitFirst)      "Omit first line"
    , Opt.Option "n" ["number"]     (Opt.NoArg OptNumber)         "Add numbering term"
    , Opt.Option ""  ["header"]     (Opt.ReqArg OptHeader "FILE") "Include header file" ]


-- --------------------------------------------  Main

type NumRecord = (Int, CSV.Record)
type JudgeType = (K.JudgePat, [K.TermName])

main :: IO ()
main = do args <- Sys.getArgs
          case Opt.getOpt Opt.Permute option args of
            (opts, args', []) 
                -> do p <- initParam opts args'
                      interact $ fromCSV p
            (_, _, errs) -> error $ show errs

fromCSV :: Param -> K.Map String
fromCSV p s =
    case CSV.parseCSV "<stdin>" s of
      Left a    -> error $ show a
      Right csv -> let js = map textJudge $ number $ omitFirst (paramOmitFirst p) csv
                   in unlines $ appendHeader (paramHeaders p) js
    where
      textJudge = K.writeDownJudge K.shortEmpty . judge n (paramJudgeType p)
      n = paramNumber p

      number :: [CSV.Record] -> [NumRecord]
      number = zip (ints 1) . K.omit (== [""])

omitFirst :: Bool -> K.Map [CSV.Record]
omitFirst omit csv
    | omit       = tail2 csv
    | otherwise  = csv

appendHeader :: [String] -> K.Map [String]
appendHeader hs body = ["** -*- koshu -*-"] ++ hs' ++ [""] ++ body where
    hs'  = map K.commentLine $ concatMap ls hs
    ls h = "" : lines h

judge :: Bool -> JudgeType -> NumRecord -> K.JudgeC
judge number (pat, ns) (num, cs)
    | number    = K.affirm pat xs'
    | otherwise = K.affirm pat xs
    where
      xs' = ("#", K.pInt num) : xs
      xs  = (map toTerm $ zip ns cs)
      toTerm (n, "") = (n, K.empty)
      toTerm (n, c)  = (n, K.pText c)


-- --------------------------------------------  Utility

ints :: Int -> [Int]
ints n = [n ..]

tail2 :: K.Map [a]
tail2 (_ : xs) = xs
tail2 []       = []

