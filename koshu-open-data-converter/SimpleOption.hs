{-# OPTIONS_GHC -Wall #-}

module SimpleOption
 ( -- * Option type
   SimpleOption (..),
   StringOptionDescr, StringOption,
   flag, opt, req,
   help, version, usage,

   -- * Processing
   ParseResult, StringResult,
   parse, parseCommand,
   getFlag,
 ) where

import qualified System.Console.GetOpt as Opt
import qualified System.Environment    as Env


-- --------------------------------------------  Simple option

data SimpleOption a
    = SimpleFlag a
    | SimpleOpt  a (Maybe String)
    | SimpleReq  a String
      deriving (Show, Eq, Ord)

type StringOptionDescr  = Opt.OptDescr StringOption
type StringOption       = SimpleOption String

flag :: [Char] -> [String] -> String -> StringOptionDescr
flag short long = Opt.Option short long $ Opt.NoArg simple where
    simple = SimpleFlag $ head long

opt :: [Char] -> [String] -> String -> String -> StringOptionDescr
opt short long name = Opt.Option short long (Opt.OptArg simple name) where
    simple = SimpleOpt $ head long

req :: [Char] -> [String] -> String -> String -> StringOptionDescr
req short long name = Opt.Option short long (Opt.ReqArg simple name) where
    simple = SimpleReq $ head long

help :: StringOptionDescr
help = flag "h" ["help"] "Show help message"

version :: StringOptionDescr
version = flag "V" ["version"] "Show version number"

usage :: [String] -> [Opt.OptDescr a] -> IO ()
usage text opts =
    do putStr $ Opt.usageInfo (unlines text ++ "OPTION") opts
       putStrLn ""


-- --------------------------------------------  Processing

type ParseResult a  = Either [String] ([a], [String])
type StringResult   = ParseResult StringOption

parse :: [Opt.OptDescr a] -> [String] -> ParseResult a
parse opts args = case Opt.getOpt Opt.Permute opts args of
               (opts', args', err) | null err  -> Right (opts', args')
                                   | otherwise -> Left err

parseCommand :: [Opt.OptDescr a] -> IO (ParseResult a)
parseCommand opts =
    do args <- Env.getArgs
       return $ parse opts args

getFlag :: [StringOption] -> String -> Bool
getFlag opts name = or $ map (getFlag1 name) opts

getFlag1 :: String -> StringOption -> Bool
getFlag1 name (SimpleFlag n) = n == name
getFlag1 _ _                 = False

