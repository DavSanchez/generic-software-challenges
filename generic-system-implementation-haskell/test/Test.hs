module Main (main) where

import qualified Input.Props as IP
import qualified Input.Spec as IS
import qualified Report.Spec as RS
import Test.Hspec (SpecWith)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (testSpecs)
-- import qualified E2E.Spec as E2E

allSpecs :: [SpecWith ()]
allSpecs =
  mconcat
    [ IS.specs
    , RS.specs
    -- , E2E.specs -- This test fails in GitHub Actions for obvious reasons (running servers, etc). Please feel free to uncomment it (and its import) for local testing.
    ]

main :: IO ()
main = do
  specs <- concat <$> mapM testSpecs allSpecs
  defaultMain
    ( testGroup
        "All Tests"
        [ testGroup "Specs" specs,
          testGroup "Props" IP.props
        ]
    )