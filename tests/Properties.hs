{-# LANGUAGE OverloadedStrings #-}
import Test.QuickCheck
import Test.HUnit((@=?),Test(TestCase),runTestTT)
import Data.List.Split(splitOn)
import Data.Char(toLower,isLower,toUpper)
import Analyzer.Checker
import Control.Applicative((<$>))
import qualified Data.List as L
import qualified Data.Text as T

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
      (x,_) <- croppedPath c
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

main = test

