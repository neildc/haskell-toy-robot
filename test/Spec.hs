import qualified Data.Maybe as Maybe
import           Lib
import           Test.Hspec
import Data.Function

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


main :: IO ()
main = hspec $ do
  describe "Tests" $ do
    it "Can be rotated left" $ do
      parseAndRun ["LEFT"] (stateAtOrigin Lib.North) `shouldBe` stateAtOrigin Lib.West

    it "Can be rotated right" $ do
      parseAndRun ["RIGHT"] (stateAtOrigin Lib.North) `shouldBe` stateAtOrigin Lib.East

    it "Can be placed" $ do
      parseAndRun ["PLACE 2,2,NORTH"] (stateAtOrigin Lib.North) `shouldBe` ((2,2), Lib.North)
