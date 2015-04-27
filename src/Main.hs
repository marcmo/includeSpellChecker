import System.Environment(getArgs)
import Analyzer.Checker
import qualified Data.ByteString as BS

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
                (\_c -> print ("fixed file " ++ h) >> BS.writeFile h _c)
                (fixIncludes c valid)


