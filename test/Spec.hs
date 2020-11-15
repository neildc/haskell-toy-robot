import           Data.Function
import qualified Data.List     as List
import qualified Data.Maybe    as Maybe
import           Lib
import           Test.Hspec

stateAtOrigin :: Lib.Direction -> State
stateAtOrigin direction =
  ((0,0), direction)

stateAtCornerOppositeOrigin :: Lib.Direction -> State
stateAtCornerOppositeOrigin direction =
  ((Lib.boardWidth - 1, Lib.boardHeight - 1), direction)

parseAndRun :: [String] -> Lib.State -> Lib.State
parseAndRun inputs initialState =
  foldl
      (flip Lib.update)
      initialState
      inputsParsed
  where
    inputsParsed =
      inputs & map Lib.parse & Maybe.catMaybes

parseAndRunOriginN :: [String] -> Lib.State
parseAndRunOriginN inputs =
  parseAndRun inputs $ stateAtOrigin Lib.North

main :: IO ()
main = hspec $ do
  describe "Tests" $ do
    it "Can be rotated left" $ do
      parseAndRunOriginN ["LEFT"] `shouldBe` stateAtOrigin Lib.West

    it "Can be rotated right" $ do
      parseAndRunOriginN ["RIGHT"] `shouldBe` stateAtOrigin Lib.East

    it "Can spin around right" $ do
      parseAndRunOriginN (List.replicate 4 "RIGHT") `shouldBe` stateAtOrigin Lib.North

    it "Can spin around left" $ do
      parseAndRun (List.replicate 4 "LEFT") (stateAtOrigin Lib.East) `shouldBe` stateAtOrigin Lib.East

    it "Can be placed" $ do
      parseAndRunOriginN  ["PLACE 2,2,EAST"] `shouldBe` ((2,2), Lib.East)
