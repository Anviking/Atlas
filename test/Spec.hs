import           Control.Exception (evaluate)
import           Lib
import           Test.Hspec


readFixture :: String -> IO String
readFixture name = readFile $ "test/fixtures/" <> name <> ".txt"

main :: IO ()
main = hspec $ do

  describe "stack build" $ do
    it "parses an error" $ do
      x <- runIO readFixture "stack-build"
      parseStack x `shouldBe` []



