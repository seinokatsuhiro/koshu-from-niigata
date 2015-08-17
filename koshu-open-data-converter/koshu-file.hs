--
--  DESCRIPTION
--    List file information
--
--  USAGE
--    koshu-file
--

{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import qualified Data.Time.Calendar             as Cal
import qualified Foreign.C.Types                as Posix
import qualified System.Posix.Types             as Posix
import qualified System.PosixCompat.Files       as Posix
import qualified System.FilePath                as Dir
import qualified System.Directory               as Dir
import qualified Koshucode.Baala.Base           as K
import qualified Koshucode.Baala.Core           as K
import qualified Koshucode.Baala.Type.Vanilla   as K

main :: IO ()
main = do putStrLn "-*- koshu -*-"
          putStrLn ""
          ps <- recursivePathList "."
          js <- mapM judgeIO $ map undot ps
          K.putJudges js

undot :: K.Map String
undot ('.' : '/' : s) = s
undot s               = s

judgeIO :: FilePath -> IO K.JudgeC
judgeIO path =
    do stat <- Posix.getFileStatus path
       return $ judge path
                  (Posix.fileSize stat)
                  (Posix.modificationTime stat)

judge :: FilePath -> Posix.COff -> Posix.CTime -> K.JudgeC
judge path (Posix.COff size) (Posix.CTime time) = K.affirm "FILE" xs
    where
      tc = case timeFromUnix time of
             Left _  -> K.empty
             Right t -> K.pTime t
      xs = [ K.term "time" $ tc
           , K.term "size" $ K.pInteger $ fromIntegral size
           , K.term "path" $ K.pText path ]

recursivePathList :: FilePath -> IO [FilePath]
recursivePathList dir =
    do names <- Dir.getDirectoryContents dir
       let properNames = filter (`notElem` [".", ".."]) names
       paths <- mapM rec properNames
       return $ concat paths
    where
      rec name =
          do let path = dir Dir.</> name
             isDirectory <- Dir.doesDirectoryExist path
             case isDirectory of
               True  -> recursivePathList path
               False -> return [path]

timeFromUnix :: (Integral n) => n -> K.Ab K.Time
timeFromUnix time =
    case K.dhmsFromSec $ fromIntegral time of
      (day, h, m, s) -> let date  = K.Monthly $ dayFromUnix day
                            clock = K.clockFromHms h m (Just s)
                        in K.timeFromDczAb date clock Nothing

dayFromUnix :: (Integral n) => n -> Cal.Day
dayFromUnix day = Cal.ModifiedJulianDay $ fromIntegral day + 40587

