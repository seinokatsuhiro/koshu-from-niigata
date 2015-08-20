--
--  DESCRIPTION
--    List file information
--
--  USAGE
--    koshu-file [OPTION]
--

{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import qualified Data.Time.Calendar              as Cal
import qualified Data.Time.LocalTime             as Time
import qualified Foreign.C.Types                 as Posix
import qualified System.Posix.Types              as Posix
import qualified System.PosixCompat.Files        as Posix
import qualified System.FilePath                 as Dir
import qualified System.Directory                as Dir
import qualified Koshucode.Baala.Base            as K
import qualified Koshucode.Baala.Core            as K
import qualified Koshucode.Baala.Type.Vanilla    as K
import qualified SimpleOption                    as Opt

import qualified Data.Version                    as Ver
import qualified Paths_koshu_open_data_converter as Ver

versionString :: String
versionString = "koshu-file-" ++ Ver.showVersion Ver.version


-- --------------------------------------------  Parameter

data Param = Param
    { paramTimeZone  :: Time.TimeZone  -- Local time zone
    , paramFile      :: Bool           -- Only files
    , paramDir       :: Bool           -- Only directories
    , paramRec       :: Bool           -- Recursive
    , paramStart     :: [String]       -- Files or directories

    , paramAddBase   :: Bool           -- Add /base term
    , paramAddExt    :: Bool           -- Add /ext term
    , paramAddDirs   :: Bool           -- Add /dirs term
    , paramCutPath   :: Bool           -- Cut /path term

    , paramHelp      :: Bool           -- Show help
    , paramVersion   :: Bool           -- Show version
    } deriving (Show, Eq, Ord)

initParam :: Opt.StringResult -> IO Param
initParam (Left errs) = error $ unwords errs
initParam (Right (opts, args)) =
    do zone   <- Time.getCurrentTimeZone
       start' <- if null args
                 then dirFiles "."  -- current directory
                 else return args   -- given files/dirs
       return $ Param { paramTimeZone = zone
                      , paramFile     = file'
                      , paramDir      = dir'
                      , paramRec      = getFlag "rec"
                      , paramStart    = start'

                      , paramAddBase  = getFlag "add-base"
                      , paramAddExt   = getFlag "add-ext"
                      , paramAddDirs  = getFlag "add-dirs"
                      , paramCutPath  = getFlag "cut-path"

                      , paramHelp     = getFlag "help"
                      , paramVersion  = getFlag "version" }
    where
      getFlag = Opt.getFlag opts

      file'  | noFileDir  = True
             | otherwise  = file
      dir'   | noFileDir  = True
             | otherwise  = dir
      file                = getFlag "file"
      dir                 = getFlag "dir"
      noFileDir           = not file && not dir

usageHeader :: [String]
usageHeader =
    [ "DESCRIPTION"
    , "  List files or directories in Koshucode"
    , ""
    , "USAGE"
    , "  koshu-file [OPTION ...]"
    , "    * List current directory"
    , "  koshu-file FILE/DIR ... [OPTION ...]"
    , "    * List FILE/DIR ..."
    , ""
    ]

options :: [Opt.StringOptionDescr]
options =
    [ Opt.help
    , Opt.version
    , Opt.flag ""  ["file"]      "List files"
    , Opt.flag ""  ["dir"]       "List directories"
    , Opt.flag "r" ["rec"]       "List recursively"
    , Opt.flag ""  ["add-base"]  "Add /base term for basename"
    , Opt.flag ""  ["add-ext"]   "Add /ext term for extension"
    , Opt.flag ""  ["add-dirs"]  "Add /dirs term for directory list"
    , Opt.flag ""  ["cut-path"]  "Cut /path term"
    ]


-- --------------------------------------------  main

main :: IO ()
main = do cmd  <- Opt.parseCommand options
          pa   <- initParam cmd
          body pa

body :: Param -> IO ()
body pa@Param { paramHelp = help, paramVersion = version }
    | help       = Opt.usage usageHeader options
    | version    = putStrLn versionString
    | otherwise  = do putStrLn "-*- koshu -*-"
                      let unk = map Unknown $ paramStart pa
                      loop 0 unk
    where
      loop :: Int -> [FileDir] -> IO ()
      loop n paths =
          do paths' <- dirExpand (paramRec pa) paths
             case paths' of
               p : ps | skip p    -> loop n ps
                      | otherwise -> do K.when (n `mod` 10 == 0) $ putStrLn ""
                                        j <- judgeIO pa p
                                        K.putJudge j
                                        loop (n + 1) ps
               [] -> do putStrLn ""
                        putStrLn $ "*** " ++ show n ++ " judges"

      both   = file && dir
      file   = paramFile pa
      dir    = paramDir  pa
      skip p | both              = False
             | file && isFile p  = False
             | file              = True
             | dir && isDir p    = False
             | dir               = True
             | otherwise         = True

judgeIO :: Param -> FileDir -> IO K.JudgeC
judgeIO pa (File path stat) = return j where
    j = judgeFile pa path
        (Posix.fileSize stat)
        (Posix.modificationTime stat)
judgeIO pa (Dir path stat n) = return j where
    j = judgeDir pa path n
        (Posix.modificationTime stat)
judgeIO _ (Unknown path) = error $ "Unknown path: " ++ path

judgeFile :: Param -> FilePath -> Posix.COff -> Posix.CTime -> K.JudgeC
judgeFile pa filepath (Posix.COff size) time = K.affirm "FILE" xs where
    xs = stem
         ++ add (paramAddExt pa) ext
         ++ add (paramAddBase pa) base
         ++ add (paramAddDirs pa) dirs
         ++ cut (paramCutPath pa) path

    stem  = [ K.term "time" $ timeFrom pa time
            , K.term "size" $ pIntegral size ]

    ext   = K.term "ext" $ K.pMaybeText extname
    base  = K.term "base" $ K.pText basename
    dirs  = K.term "dirs" $ K.pList $ map K.pText $ dirsFrom filepath
    path  = K.term "path" $ K.pText filepath

    basename  = Dir.takeBaseName  filepath
    extname   = undot $ Dir.takeExtension filepath

    add True  a   = [a]
    add False _   = []

    cut True  _   = []
    cut False a   = [a]

dirsFrom :: FilePath -> [FilePath]
dirsFrom = cutFirst "" . K.divideBy Dir.isPathSeparator . undot . Dir.takeDirectory

undot :: K.Map String
undot = cutFirst '.'

cutFirst :: (Eq a) => a -> K.Map [a]
cutFirst k (x:xs) | x == k  = xs
cutFirst _ xs               = xs

judgeDir :: Param -> FilePath -> Int -> Posix.CTime -> K.JudgeC
judgeDir pa path n time = K.affirm "DIR" xs where
    xs = [ K.term "time"  $ timeFrom pa time
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

dirExpand :: Bool -> [FileDir] -> IO [FileDir]
dirExpand rec (Unknown p : ps) =
    do stat <- Posix.getFileStatus p
       case Posix.isDirectory stat of
         False -> return $ File p stat : ps
         True  -> do ns <- dirFiles p
                     let ns' | rec       = p // ns
                             | otherwise = []
                         dir = Dir p stat (length ns)
                         pps = (Unknown `map` ns') ++ ps
                     return $ dir : pps
         -- Input   [Unknown P1, Unknown P2, Unknown P3]
         -- Output  [File P1, Unknown P2, Unknown P3]
         --         [Dir P1, File P4, File P5, Unknown P2, Unknown P3]
dirExpand _ ps = return ps

dirFiles :: FilePath -> IO [FilePath]
dirFiles path =
    do ns <- Dir.getDirectoryContents path
       let ns' = K.omit (`elem` [".", ".."]) ns
       return ns'

-- --------------------------------------------  Timestamp

timeFrom :: Param -> Posix.CTime -> K.VContent
timeFrom pa (Posix.CTime time) = tc where
    zone = Just $ 60 * (Time.timeZoneMinutes $ paramTimeZone pa)
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

