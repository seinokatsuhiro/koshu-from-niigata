--
--  DESCRIPTION
--    List file information
--
--  USAGE
--    koshu-file [OPTION]
--

{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import qualified Data.Time.Calendar             as Cal
import qualified Data.Time.LocalTime            as Time
import qualified Foreign.C.Types                as Posix
import qualified System.Posix.Types             as Posix
import qualified System.PosixCompat.Files       as Posix
import qualified System.FilePath                as Dir
import qualified System.Directory               as Dir
import qualified Koshucode.Baala.Base           as K
import qualified Koshucode.Baala.Core           as K
import qualified Koshucode.Baala.Type.Vanilla   as K
import qualified SimpleOption                   as Opt


-- --------------------------------------------  Parameter

data Param = Param
    { paramTimeZone :: Time.TimeZone
    , paramFile     :: Bool
    , paramDir      :: Bool
    } deriving (Show, Eq, Ord)

initParam :: Opt.StringResult -> IO Param
initParam (Left errs) = error $ unwords errs
initParam (Right (opts, _)) =
    do zone <- Time.getCurrentTimeZone
       return $ Param { paramTimeZone = zone
                      , paramFile     = file'
                      , paramDir      = dir' }
    where
      getFlag = Opt.getFlag opts

      file' | noFileDir  = True
            | otherwise  = file
      dir'  | noFileDir  = True
            | otherwise  = dir
      file               = getFlag "file"
      dir                = getFlag "dir"
      noFileDir          = not file && not dir

options :: [Opt.StringOptionDescr]
options =
    [ Opt.help
    , Opt.version
    , Opt.flag ""  ["file"]    "List files"
    , Opt.flag ""  ["dir"]     "List directories"
    ]


-- --------------------------------------------  main

main :: IO ()
main = do cmd    <- Opt.parseCommand options
          param  <- initParam cmd
          putStrLn "-*- koshu -*-"
          body param 0 [Unknown "."]

body :: Param -> Int -> [FileDir] -> IO ()
body param = loop where
    loop n paths =
        do paths' <- dirExpand paths
           case paths' of
             p : ps | skip p    -> body param n ps
                    | otherwise -> do K.when (n `mod` 10 == 0) $ putStrLn ""
                                      j <- judgeIO param p
                                      K.putJudge j
                                      body param (n + 1) ps
             [] -> do putStrLn ""
                      putStrLn $ "*** " ++ show n ++ " judges"

    both   = file && dir
    file   = paramFile param
    dir    = paramDir  param
    skip p | both              = False
           | file && isFile p  = False
           | file              = True
           | dir && isDir p    = False
           | dir               = True
           | otherwise         = True

judgeIO :: Param -> FileDir -> IO K.JudgeC
judgeIO param (File path stat) = return j where
    j = judgeFile param path
        (Posix.fileSize stat)
        (Posix.modificationTime stat)
judgeIO param (Dir path stat n) = return j where
    j = judgeDir param path n
        (Posix.modificationTime stat)
judgeIO _ (Unknown path) = error $ "Unknown path: " ++ path

judgeFile :: Param -> FilePath -> Posix.COff -> Posix.CTime -> K.JudgeC
judgeFile param path (Posix.COff size) time = K.affirm "FILE" xs where
    xs = [ K.term "time" $ timeFrom param time
         , K.term "size" $ pIntegral size
         , K.term "path" $ K.pText path ]

judgeDir :: Param -> FilePath -> Int -> Posix.CTime -> K.JudgeC
judgeDir param path n time = K.affirm "DIR" xs where
    xs = [ K.term "time"  $ timeFrom param time
         , K.term "count" $ K.pInt n
         , K.term "path"  $ K.pText path ]

pIntegral :: (Integral n) => n -> K.VContent
pIntegral = K.pInteger . fromIntegral


-- --------------------------------------------  File and Directory

data FileDir
    = File    FilePath Posix.FileStatus
    | Dir     FilePath Posix.FileStatus Int
    | Unknown FilePath

isFile :: FileDir -> Bool
isFile (File _ _) = True
isFile _          = False

isDir :: FileDir -> Bool
isDir (Dir _ _ _) = True
isDir _           = False

(//) :: FilePath -> K.Map [FilePath]
"." // ns = ns
dir // ns = map (dir Dir.</>) ns

dirExpand :: [FileDir] -> IO [FileDir]
dirExpand (Unknown p : ps) =
    do stat <- Posix.getFileStatus p
       case Posix.isDirectory stat of
         False -> return $ File p stat : ps
         True  -> do ns <- dirFiles p
                     let ns' = p // ns
                         dir = Dir p stat (length ns)
                         pps = (Unknown `map` ns') ++ ps
                     return $ dir : pps
         -- Input   [Unknown P1, Unknown P2, Unknown P3]
         -- Output  [File P1, Unknown P2, Unknown P3]
         --         [Dir P1, File P4, File P5, Unknown P2, Unknown P3]
dirExpand ps = return ps

dirFiles :: FilePath -> IO [FilePath]
dirFiles path =
    do ns <- Dir.getDirectoryContents path
       let ns' = K.omit (`elem` [".", ".."]) ns
       return ns'

-- --------------------------------------------  Timestamp

timeFrom :: Param -> Posix.CTime -> K.VContent
timeFrom param (Posix.CTime time) = tc where
    zone = Just $ 60 * (Time.timeZoneMinutes $ paramTimeZone param)
    tc = case timeFromUnix time zone of
           Left _  -> K.empty
           Right t -> K.pTime t

timeFromUnix :: (Integral n) => n -> Maybe K.Sec -> K.Ab K.Time
timeFromUnix time zone =
    case K.dhmsFromSec $ fromIntegral time of
      (day, h, m, s) -> let date  = K.Monthly $ dayFromUnix day
                            clock = K.clockFromHms h m (Just s)
                        in K.timeFromDczAb date clock zone

dayFromUnix :: (Integral n) => n -> Cal.Day
dayFromUnix day = Cal.ModifiedJulianDay $ fromIntegral day + 40587

