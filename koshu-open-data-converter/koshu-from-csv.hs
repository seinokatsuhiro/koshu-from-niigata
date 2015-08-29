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

import qualified SimpleOption                    as Opt
import qualified Text.CSV                        as CSV
import qualified Koshucode.Baala.Base            as K
import qualified Koshucode.Baala.Core            as K
import qualified Koshucode.Baala.Type.Vanilla    as K
import qualified Paths_koshu_open_data_converter as Ver


-- --------------------------------------------  Main

versionString :: String
versionString = "koshu-from-csv-" ++ Opt.showVersion Ver.version

usageHeader :: [String]
usageHeader =
    [ "DESCRIPTION"
    , "  Convert CSV to Koshucode"
    , ""
    , "USAGE"
    , "  koshu-csv-file [OPTION] < FILE.csv"
    , "    * Convert to |-- CSV /1 ... /2 ..."
    , "  koshu-csv-file PAT /N ... [OPTION] < FILE.csv"
    , "    * Convert to |-- PAT /N ..."
    , ""
    ]

options :: [Opt.StringOptionDescr]
options =
    [ Opt.help
    , Opt.version
    , Opt.flag "1" ["omit-first"]         "Omit first line"
    , Opt.flag "n" ["number"]             "Add numbering term"
    , Opt.req  ""  ["input"]   "FILE"     "CSV file"
    , Opt.req  ""  ["header"]  "FILE"     "Include header file"
    , Opt.req  ""  ["judge"]   "FILE"     "Judgement file"
    , Opt.req  ""  ["license"] "FILE"     "Include license file"
    , Opt.flag ""  ["trim", "trim-both"]  "Trim spaces left and right"
    , Opt.flag ""  ["trim-left"]          "Trim spaces left side"
    , Opt.flag ""  ["trim-right"]         "Trim spaces right side"
    ]

main :: IO ()
main = do cmd <- Opt.parseCommand options
          p   <- initParam cmd
          mainParam p

mainParam :: Param -> IO ()
mainParam p@Param { paramHelp = help, paramVersion = version }
     | help       = Opt.usage usageHeader options
     | version    = putStrLn versionString
     | otherwise  = edit (fromCSV p) $ paramInput p


-- --------------------------------------------  Param

data Param = Param
    { paramOmitFirst  :: Bool
    , paramNumber     :: Bool
    , paramTrim       :: (Bool, Bool)
    , paramInput      :: [FilePath]
    , paramHeaders    :: [String]
    , paramLicenses   :: [String]
    , paramJudgeType  :: JudgeType

    , paramHelp       :: Bool
    , paramVersion    :: Bool
    } deriving (Show, Eq, Ord)

initParam :: Opt.StringResult -> IO Param
initParam (Left errs) = error $ unwords errs
initParam (Right (opts, args)) =
    do K.useUtf8 K.stdout
       hs <- readFiles $ getReq "header"
       js <- readFiles $ getReq "judge"
       ls <- readFiles $ getReq "license"
       let as = K.omit null $ map K.trimBoth $ args ++ js
       return $ Param { paramOmitFirst  = getFlag "omit-first"
                      , paramNumber     = getFlag "number"
                      , paramTrim       = trim
                      , paramInput      = getReq "input"
                      , paramHeaders    = hs
                      , paramLicenses   = ls
                      , paramJudgeType  = parseJudgeType as

                      , paramHelp       = getFlag "help"
                      , paramVersion    = getFlag "version" }
    where
      getFlag  = Opt.getFlag opts
      getReq   = Opt.getReq  opts

      trim | getFlag "trim"       = (True, True)
           | getFlag "trim-both"  = (True, True)
           | getFlag "trim-left"  = (True, False)
           | getFlag "trim-right" = (False, True)
           | otherwise            = (False, False)

readFiles :: [FilePath] -> IO [String]
readFiles paths =
    do contents <- mapM readFile paths
       return $ concatMap lines contents

parseJudgeType :: [String] -> JudgeType
parseJudgeType [] = ("CSV", map show $ ints 1)
parseJudgeType (pat@('/' : _) : _) = error $ "Pattern like term name: " ++ pat
parseJudgeType (pat : ns) = (pat, map name ns) where
    name ('/' : n) = n
    name n         = error $ "Not a term: " ++ n


-- --------------------------------------------  Edit

type NumRecord = (Int, CSV.Record)
type JudgeType = (K.JudgePat, [K.TermName])

edit :: K.Map String -> [FilePath] -> IO ()
edit f [] = interact f
edit f ps = editFile f `mapM_` ps

editFile :: K.Map String -> FilePath -> IO ()
editFile f path = do content <- readFile path
                     putStr $ f content

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
    body'     = ["** -*- koshu -*-"] ++ header' ++ [""] ++ license' ++ body
    pad s     = "  " ++ s
    header'   | null header  = []
              | otherwise    = map K.commentLine $ "" : (shorten header)
    license'  | null license = []
              | otherwise    = shorten $
                                 ["=== license", ""]
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

