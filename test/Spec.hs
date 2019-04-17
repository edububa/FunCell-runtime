import Test.Hspec
import Algebra.Graph.AdjacencyMap hiding (empty)
import Lib.Dependency

main :: IO ()
main = hspec $ do
         describe "Lib.Dependency.addDependency" $ do
                 it "adds a dependency" $ do
                   (addDependency (0,0) (0,1) $ empty) `shouldBe` (edge (0,0) (0,1))
