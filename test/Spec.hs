{-# OPTIONS_GHC -Wall #-}
import Lib
import Test.HUnit (runTestText, test, putTextToHandle, Test (TestList), (~:), (~?=))
import System.IO (stderr)
import Control.Monad (void)
import Data.Tuple.Extra (second, dupe)

testList :: [String] -> IO Test
testList = (TestList <$>) . mapM (\x -> do
    (pubkey, prikey) <- uncurry genKeys . second rootFromSafePrime . dupe =<< genRndSafePrime 32
    encoded <- encode pubkey x
    let (Just res) = decode (pubkey, prikey) encoded
    return $ test $ "Converting test: " ~: res ~?= x)

runTest :: Test -> IO ()
runTest = void . runTestText (putTextToHandle stderr False)
    
main :: IO ()
main = runTest =<< testList ["hoge", "foo", "bar"]
