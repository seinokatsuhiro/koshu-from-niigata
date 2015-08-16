--
--  DESCRIPTION
--    Convert CSV to Koshucode
--
--  USAGE
--    koshu-from-csv < FILE.csv
--      # convert to |-- CSV /1 /2 ...
--    koshu-from-csv PAT /N /N ... < FILE.csv
--      # convert to |-- PAT /N /N ...
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
    , paramTrim       :: (Bool, Bool)
    , paramHeaders    :: [String]
    , paramLicenses   :: [String]
    , paramJudgeType  :: JudgeType
    } deriving (Show, Eq, Ord)

initParam :: [Option] -> [String] -> IO Param
initParam opts args =
    do K.useUtf8 K.stdout
       hs <- readFileFor optHeader  opts
       js <- readFileFor optJudge   opts
       ls <- readFileFor optLicense opts
       let as = K.omit null $ map K.trimBoth $ args ++ js
       return $ Param { paramOmitFirst  = OptOmitFirst `elem` opts
                      , paramNumber     = OptNumber    `elem` opts
                      , paramTrim       = trim
                      , paramHeaders    = hs
                      , paramLicenses   = ls
                      , paramJudgeType  = parseJudgeType as }
    where
      trim = case K.unique $ K.catMaybes $ map optTrim opts of
               [lr] -> lr
               []   -> (False, False)
               _    -> error "Ambiguous trimming"

readFileFor :: (Option -> Maybe String) -> [Option] -> IO [String]
readFileFor select opts =
    case K.mapMaybe select opts of
      []     -> return []
      files  -> do contents <- mapM readFile files
                   return $ concatMap lines contents

parseJudgeType :: [String] -> JudgeType
parseJudgeType [] = ("CSV", map show $ ints 1)
parseJudgeType (pat@('/' : _) : _) = error $ "Pattern like term name: " ++ pat
parseJudgeType (pat : ns) = (pat, map name ns) where
    name ('/' : n) = n
    name n         = error $ "Not a term: " ++ n


-- --------------------------------------------  Option

data Option
    = OptHelp                   -- ^ Show help
    | OptVersion                -- ^ Show version number
    | OptOmitFirst              -- ^ Omit first line from CSV daat
    | OptNumber                 -- ^ Add numbering term
    | OptHeader  String         -- ^ Header file
    | OptJudge   String         -- ^ Judgement file
    | OptLicense String         -- ^ License file
    | OptTrim    Bool Bool      -- ^ Triming spaces left or right
      deriving (Show, Eq, Ord)

optTrim :: Option -> Maybe (Bool, Bool)
optTrim (OptTrim left right) = Just (left, right)
optTrim _                    = Nothing

optHeader :: Option -> Maybe String
optHeader (OptHeader s)   = Just s
optHeader (_)             = Nothing

optJudge :: Option -> Maybe String
optJudge (OptJudge s) = Just s
optJudge (_)          = Nothing

optLicense :: Option -> Maybe String
optLicense (OptLicense s) = Just s
optLicense (_)            = Nothing

option :: [Opt.OptDescr Option]
option =
    [ Opt.Option "1" ["omit-first"] (Opt.NoArg OptOmitFirst)       "Omit first line"
    , Opt.Option "n" ["number"]     (Opt.NoArg OptNumber)          "Add numbering term"
    , Opt.Option ""  ["header"]     (Opt.ReqArg OptHeader  "FILE") "Include header file"
    , Opt.Option ""  ["judge"]      (Opt.ReqArg OptJudge   "FILE") "Judgement file"
    , Opt.Option ""  ["license"]    (Opt.ReqArg OptLicense "FILE") "Include license file"
    , Opt.Option ""  ["trim", "trim-both"]
                                    (Opt.NoArg $ OptTrim True True) "Trim spaces left and right"
    , Opt.Option ""  ["trim-left"]  (Opt.NoArg $ OptTrim True False) "Trim spaces left side"
    , Opt.Option ""  ["trim-right"] (Opt.NoArg $ OptTrim False True) "Trim spaces right side" ]


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
      Right csv -> let js = map textJudge $ number $ trim $ omitFirst (paramOmitFirst p) csv
                   in unlines $ appendHeader (paramHeaders p) (paramLicenses p) js
    where
      textJudge = K.writeDownJudge K.shortEmpty . judge n (paramJudgeType p)
      n = paramNumber p

      number :: [CSV.Record] -> [NumRecord]
      number = zip (ints 1) . K.omit (== [""])

      trim :: K.Map [CSV.Record]
      trim = case paramTrim p of
               (False, False) -> id
               (True, False)  -> K.map2 K.trimLeft
               (False, True)  -> K.map2 K.trimRight
               (True, True)   -> K.map2 K.trimBoth 

omitFirst :: Bool -> K.Map [CSV.Record]
omitFirst omit csv
    | omit       = tail2 csv
    | otherwise  = csv

appendHeader :: [String] -> [String] ->  K.Map [String]
appendHeader header license body = body' where
    body'     = ["** -*- koshu -*-"] ++ header' ++ [""] ++ shorten license' ++ body
    header'   = map K.commentLine $ concatMap ls header
    ls h      = "" : lines h
    pad s     = "  " ++ s
    license'  | null license = []
              | otherwise    = ["=== license", ""]
                               ++ map pad license
                               ++ ["", "=== rel", ""]

shorten :: K.Map [String]
shorten (x1 : x2 : xs)
    | null x1' && null x2' = shorten (x2' : xs)
    | otherwise            = x1 : shorten (x2 : xs)
    where x1' = K.trimLeft x1
          x2' = K.trimLeft x2
shorten (x1 : xs) = x1 : shorten xs
shorten [] = []

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

