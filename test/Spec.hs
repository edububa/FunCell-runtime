import Test.Hspec
import ParsingLib

main :: IO ()
main = hspec $ do
  it "Parses an empty string" $ do
    parseAndEval "" `shouldBe` (Right "")
  it "Parses and eval \"1 + 2\"" $ do
    parseAndEval "1 + 2" `shouldBe` (Right "3")
