{-# LANGUAGE OverloadedStrings #-}
import System.FilePath.Find
import System.FilePath.Posix
import qualified Data.ByteString as BS
import qualified Data.Map as M
import Control.Applicative
import qualified Data.ByteString.Char8 as BSC
import Text.Regex
import Data.Maybe(isJust)
import System.Environment(getArgs)
import qualified Data.List as L
import Data.List(foldl')
import Data.List.Split(splitOn)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Test.QuickCheck
import Test.HUnit((@=?),Test(TestCase),runTestTT)
import Data.Char(toLower,isLower,toUpper)
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
member (x:xs) (Node b tr) = maybe Nothing (member xs) (M.lookup x tr)

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
fixIncludes content availablePaths
  | n == 0 = Nothing
  | otherwise = Just $ BSC.unlines $ reverse finalLines
  where allLines = BSC.lines content
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


main = do
  (p:ws:_) <- getArgs
  hss <- headerFiles ws
  let validCaseTrie = toTrie hss
  headersAndSources <- headerAndSourceFiles p
  mapM_ (fixFile validCaseTrie) headersAndSources
    where fixFile valid h = do
              c <- BS.readFile h
              maybe
                (putStr ".")
                (\c -> print ("fixed file " ++ h) >> BS.writeFile h c)
                (fixIncludes c valid)

-- tests
--
test = runTestTT $ TestCase $ Just "a/b/TEST.h" @=?
    bestMatch "a/b/test.h" (toTrie ["b/TEST.h","a/b/c.h","/one/a/b/TEST.h"])

data Path = PA { getPath :: String } deriving (Eq,Show)
instance Arbitrary Path where
  arbitrary = PA <$> L.intercalate "/" <$> resize 10 (listOf1 seg)
    where seg = resize 20 $ listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'])

prop_findBetterMatch :: [Path] -> Property
prop_findBetterMatch ps = length ps > 1 ==>
   do let xx@(c:_) = map getPath ps
      (x,n) <- croppedPath c
      return $ bestMatch (changeCase (T.pack x)) (toTrie xx) == Just (T.pack x)

data IncludeString = IS { getString :: (String,String) } deriving (Eq,Show)
instance Arbitrary IncludeString where
  arbitrary = do
    (PA s) <- arbitrary :: Gen Path
    (open,close) <- elements [("<",">"),("\"","\"")]
    rest <- arbitrary
    return $ IS (s, "#include " ++ open ++ s ++ close ++ rest)

prop_cutIncludePath (IS (s,full)) =
  let (core,_,_) = cutIncludePath (T.pack full) in
  T.unpack core == s

croppedPath ::  String -> Gen (String,Int)
croppedPath p = do
  let choppedInc = splitOn "/" p
  n <- choose (0,length choppedInc -1)
  return (L.intercalate "/" (drop n choppedInc),n)

changeCase :: T.Text -> T.Text
changeCase = T.pack . map changed . T.unpack
  where changed c = if isLower c then toUpper c else toLower c


