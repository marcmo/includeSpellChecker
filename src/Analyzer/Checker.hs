{-# LANGUAGE OverloadedStrings #-}
module Analyzer.Checker where

import System.FilePath.Find
import System.FilePath.Posix
import qualified Data.ByteString as BS
import qualified Data.Map as M
import Control.Applicative
import qualified Data.ByteString.Char8 as BSC
import Text.Regex
import Data.Maybe(isJust)
import qualified Data.List as L
import Data.List(foldl')
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
-- import Debug.Trace
-- trc xs = trace (concat xs) -- debug on
trc _ a = a -- debug off

type TrieMap a = M.Map a (Trie a)
data Trie a = Empty | Node (Maybe [a]) (TrieMap a) deriving (Show)

insert :: (Eq a,Ord a) => [a] -> Trie a -> Trie a
insert = insert' [] where
  insert' rs c Empty = insert' rs c (Node Nothing M.empty)
  insert' rs [] (Node _ d) = Node (Just rs) d
  insert' rs (x:xs) (Node b dic)
    | x `M.member` dic = Node b (M.adjust (insert' (x:rs) xs) x dic)
    | otherwise = Node b (M.insert x (insert' (x:rs) xs Empty) dic)

member :: (Ord a,Show a) => [a] -> Trie a -> Maybe [a]
member _ Empty = Nothing
member [] (Node b _) = b
member (x:xs) (Node _ tr) = maybe Nothing (member xs) (M.lookup x tr)

fromList = foldr insert Empty

(=~) ::  String -> String -> Bool
(=~) inp pat = isJust $ matchRegex (mkRegex pat) inp

headerFiles = find always (extension ==? ".h")
headerAndSourceFiles = find always (extension ==? ".h" ||? extension ==? ".c")

newtype CaseInsensitive = CI { content :: T.Text }
instance Eq CaseInsensitive where
  (==) (CI a) (CI b) = T.toCaseFold a == T.toCaseFold b
instance Ord CaseInsensitive where
  compare (CI a) (CI b) = compare (T.toCaseFold a) (T.toCaseFold b)
instance Show CaseInsensitive where show (CI a) = T.unpack a

toTrie ::  [String] -> Trie CaseInsensitive
toTrie hss = fromList [map CI ts | ts <- validPaths]
  where allPaths = map T.pack hss
        validPaths = concatMap reverseSplitAll allPaths
        reverseSplitAll = map reverse . L.tails . splitP
        splitP = map T.pack . splitDirectories . T.unpack

type Include = (Int,BS.ByteString)

cutIncludePath :: T.Text -> (T.Text,T.Text,T.Text)
cutIncludePath i
    | T.unpack i =~ "^#include[ ]{1,}\"" = ((head . tail . T.split (== '"')) i,"\"","\"")
    | T.unpack i =~ "^#include[ ]{1,}<"  =
      (T.dropAround (\c->(c == '<') || (c == '>')) (fst $ T.breakOn ">" $ snd $ T.breakOn "<" i),"<",">")
    | otherwise = error $ "no match for include path" ++ T.unpack i

fixIncludes :: BSC.ByteString -> Trie CaseInsensitive -> Maybe BSC.ByteString
fixIncludes _content availablePaths
  | n == 0 = Nothing
  | otherwise = Just $ BSC.unlines $ reverse finalLines
  where allLines = BSC.lines _content
        (finalLines,n) = foldl' maybeFixLine ([],0) allLines
        maybeFixLine (acc,nn) x =
          maybe (x:acc,nn)
                (\f->(E.encodeUtf8 f:acc,nn+1))
                (fixForLine x)
        fixForLine x
          | BSC.unpack x =~ "^#include[ ]{1,}[<\"]" =
              m >>= \s -> return (T.concat ["#include ",start,s,end])
          | otherwise = Nothing
          where m = bestMatch cut availablePaths
                (cut,start,end) = cutIncludePath (E.decodeUtf8 x)

-- | Finds the correct cased match for the include string
-- takes the include string and a list of possible file paths
-- e.g., if include = "a/bin/test.h" and the possible file paths are
-- ["a/BIN/Test.h","bin/Test.h"], it will result in Just "a/BIN/Test.h"
bestMatch ::  T.Text -> Trie CaseInsensitive -> Maybe T.Text
bestMatch include validCaseTrie = member reversedIncludeCI validCaseTrie >>=
    \matchedPath ->
      if (content <$> matchedPath) == (content <$> includeCI)
        then Nothing -- include path was spelled correctly
        else Just $ T.intercalate "/" [content x | x <- matchedPath]
    where
      reversedIncludeCI = map CI (reverse $ splitP include)
      includeCI = map CI (splitP include)
      splitP = map T.pack . splitDirectories . T.unpack


