{-# LANGUAGE OverloadedStrings #-}
import System.FilePath.FindCompat((||?),always,extension,find,(==?))
import qualified Data.ByteString as BS
import Control.Applicative((<$>))
import qualified Data.ByteString.Char8 as BSC
import Text.Regex
import Maybe(isJust,fromJust)
import System(getArgs)
import qualified Data.List as L
import Data.List(foldl')
import Data.Char(toLower)

(=~) ::  String -> String -> Bool
(=~) inp pat = isJust $ matchRegex (mkRegex pat) inp 

headerFiles = find always (extension ==? ".h")
headerAndSourceFiles = find always (extension ==? ".h" ||? extension ==? ".c")

main = do
  (p:ws:_) <- getArgs
  hss <- headerFiles ws
  headersAndSources <- headerAndSourceFiles p
  mapM_ (fixIncludes hss) headersAndSources

fixIncludes :: [FilePath] -> FilePath -> IO ()
fixIncludes allPaths p = do
  allLines <- BSC.lines <$> BS.readFile p
  BS.writeFile p (BSC.unlines $ map fixLine allLines)
    where fixLine x = if BSC.unpack x =~ "^#include\\s"
                        then correctCases x
                        else x
          correctCases x = let include = (BSC.unpack . head . tail . BSC.splitWith (== '"')) x in
              BSC.pack $ "#include \"" ++ findMatching include ++ "\""
          findMatching :: FilePath -> FilePath
          findMatching i = let mm = L.findIndex (== cI) cAllPaths in
              if isJust mm then allPaths!!fromJust mm else i
                where cI = map toLower i
                      cAllPaths = [map toLower p | p <- allPaths]

