import           Data.Function
import qualified Data.List     as List
import qualified Data.Either    as Either
import           Lib
import           Test.Hspec

stateAtOrigin :: Lib.Direction -> State
stateAtOrigin direction =
  ((0,0), direction)

stateAtCornerOppositeOrigin :: Lib.Direction -> State
stateAtCornerOppositeOrigin direction =
  ((Lib.boardWidth - 1, Lib.boardHeight - 1), direction)

parseAndRun :: Lib.State -> [String] -> Lib.State
parseAndRun initialState inputs  =
  foldl
      (flip Lib.update)
      initialState
      inputsParsed
  where
    inputsParsed =
      inputs & map Lib.parse & Either.rights

parseAndRunOriginN :: [String] -> Lib.State
parseAndRunOriginN  =
  parseAndRun $ stateAtOrigin Lib.North

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
      parseAndRun (stateAtOrigin Lib.East)  (List.replicate 4 "LEFT") `shouldBe` stateAtOrigin Lib.East

    it "Ignores invalid place command (invalid coordinate)" $ do
      parseAndRunOriginN  ["PLACE a,2,EAST"] `shouldBe` stateAtOrigin Lib.North

    it "Ignores invalid place command (missing direction)" $ do
      parseAndRunOriginN  ["PLACE 0,2"] `shouldBe` stateAtOrigin Lib.North

    it "Ignores invalid place command (invalid direction)" $ do
      parseAndRunOriginN  ["PLACE 0,2,CAT"] `shouldBe` stateAtOrigin Lib.North

    it "Ignores invalid place command (invalid format)" $ do
      parseAndRunOriginN  ["PLACE 02NORTH"] `shouldBe` stateAtOrigin Lib.North

    it "Can be placed" $ do
      parseAndRunOriginN  ["PLACE 2,2,EAST"] `shouldBe` ((2,2), Lib.East)

    it "Can be placed" $ do
      parseAndRunOriginN  ["PLACE 4,2,NORTH"] `shouldBe` ((4,2), Lib.North)

    it "Can't be placed out of bounds" $ do
      parseAndRunOriginN ["PLACE 7,7,EAST"] `shouldBe` stateAtOrigin Lib.North

    it "Can't be placed out of bounds, after being placed somewhere valid" $ do
      parseAndRunOriginN ["PLACE 2,2,WEST", "PLACE 7,7,EAST"] `shouldBe` ((2,2), Lib.West)

    it "Can slide east" $ do
      parseAndRun ((0,0), Lib.East) ["MOVE"] `shouldBe` ((1,0), Lib.East)

    it "Can slide west" $ do
      parseAndRun ((2,2), Lib.West) ["MOVE"] `shouldBe` ((1,2), Lib.West)

    it "Can slide north" $ do
      parseAndRun ((2,2), Lib.North) ["MOVE"] `shouldBe` ((2,3), Lib.North)

    it "Can slide south" $ do
      parseAndRun ((2,2), Lib.South) ["MOVE"] `shouldBe` ((2,1), Lib.South)

    it "Doesn't slide off west boundary" $ do
      parseAndRun ((0,0), Lib.West) ["MOVE"] `shouldBe` ((0,0), Lib.West)

    it "Doesn't slide off east boundary" $ do
      parseAndRun ((4,0), Lib.East) ["MOVE"] `shouldBe` ((4,0), Lib.East)

    it "Doesn't slide off north boundary" $ do
      parseAndRun ((0,4), Lib.North) ["MOVE"] `shouldBe` ((0,4), Lib.North)

    it "Doesn't slide off south boundary" $ do
      parseAndRun ((0,0), Lib.South) ["MOVE"] `shouldBe` ((0,0), Lib.South)

    it "Can reach the opposite corner" $ do
        (\cmds -> parseAndRunOriginN cmds `shouldBe` (stateAtCornerOppositeOrigin Lib.East)) $
            List.concat
              [ List.replicate (Lib.boardWidth - 1) "MOVE"
              , ["RIGHT"]
              , List.replicate (Lib.boardHeight - 1) "MOVE"
              ]

    it "Can reach the opposite corner and return to origin" $ do
      (\cmds -> parseAndRunOriginN cmds `shouldBe` (stateAtOrigin Lib.South)) $
          List.concat
            [ ["RIGHT"]
            , List.replicate (Lib.boardWidth - 1) "MOVE"
            , ["LEFT"]
            , List.replicate (Lib.boardHeight - 1) "MOVE"
            , ["LEFT"]
            , List.replicate (Lib.boardWidth - 1) "MOVE"
            , ["LEFT"]
            , List.replicate (Lib.boardWidth - 1) "MOVE"
            ]
