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
   getFlag, getOpt, getReq,
 ) where

import qualified Data.Maybe            as Maybe
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

getFlag :: (Eq a) => [SimpleOption a] -> a -> Bool
getFlag opts name = or $ map (getFlag1 name) opts

getOpt :: (Eq a) => [SimpleOption a] -> a -> [String]
getOpt opts name = getOpt1 name `Maybe.mapMaybe` opts

getReq :: (Eq a) => [SimpleOption a] -> a -> [String]
getReq opts name = getReq1 name `Maybe.mapMaybe` opts

getFlag1 :: (Eq a) => a -> SimpleOption a -> Bool
getFlag1 name (SimpleFlag n) = (n == name)
getFlag1 _ _ = False

getOpt1 :: (Eq a) => a -> SimpleOption a -> Maybe String
getOpt1 name (SimpleOpt n (Just arg)) | (n == name) = Just arg
getOpt1 _ _ = Nothing

getReq1 :: (Eq a) => a -> SimpleOption a -> Maybe String
getReq1 name (SimpleReq n arg) | (n == name) = Just arg
getReq1 _ _ = Nothing

