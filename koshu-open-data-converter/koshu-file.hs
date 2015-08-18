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
          body 0 ["."]

body :: Int -> [FilePath] -> IO ()
body n paths =
    do paths' <- dirExpand paths
       case paths' of
         p:ps -> do K.when (n `mod` 10 == 0) $ putStrLn ""
                    j <- judgeIO p
                    K.putJudge j
                    body (n + 1) ps

         []   -> do putStrLn ""
                    putStrLn $ "*** " ++ show n ++ " judges"

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

dirExpand :: [FilePath] -> IO [FilePath]
dirExpand [] = return []
dirExpand pps@(p:ps) =
    do dir <- Dir.doesDirectoryExist p
       case dir of
         False -> return pps
         True  -> do ns <- dirFiles p
                     dirExpand $ (p // ns) ++ ps
    where "." // ns = ns
          dir // ns = map (dir Dir.</>) ns

dirFiles :: FilePath -> IO [FilePath]
dirFiles path =
    do ns <- Dir.getDirectoryContents path
       let ns' = K.omit (`elem` [".", ".."]) ns
       return ns'

timeFromUnix :: (Integral n) => n -> K.Ab K.Time
timeFromUnix time =
    case K.dhmsFromSec $ fromIntegral time of
      (day, h, m, s) -> let date  = K.Monthly $ dayFromUnix day
                            clock = K.clockFromHms h m (Just s)
                        in K.timeFromDczAb date clock Nothing

dayFromUnix :: (Integral n) => n -> Cal.Day
dayFromUnix day = Cal.ModifiedJulianDay $ fromIntegral day + 40587

