module Report.Spec (specs) where

import GHC.Conc (atomically, readTVarIO)
import qualified Internal.Report as R
import Test.Hspec (SpecWith, describe, it, shouldBe)

specs :: [SpecWith ()]
specs =
  [ spec_report,
    spec_reportSTM
  ]

spec_report :: SpecWith ()
spec_report = describe "Report generation and modification" $ do
  it "Modifies reports (add duplicate only)" $ do
    let report = R.ReportData 0 0 0
    R.addDuplicate report `shouldBe` R.ReportData 0 1 0
    (R.addDuplicate . R.addDuplicate) report `shouldBe` R.ReportData 0 2 0
  it "Modifies reports (adds unique)" $ do
    let report = R.ReportData 1 0 0
    (R.addUnique . R.addUnique) report `shouldBe` R.ReportData 3 0 0
  it "Modifies reports (adds unique total only)" $ do
    let report = R.ReportData 1 0 1
    R.addUniqueTotal report `shouldBe` R.ReportData 1 0 2
  it "Modifies reports (adds uniques and total)" $ do
    let report = R.ReportData 1 0 1
    R.addUniqueAndTotal report `shouldBe` R.ReportData 2 0 2
  it "Resets the report to zero except the unique total" $ do
    let report = R.ReportData 1 2 3
    R.resetReport report `shouldBe` R.ReportData 0 0 3

spec_reportSTM :: SpecWith ()
spec_reportSTM = describe "Modifies the report when in the STM monad" $ do
  it "STM: Creates an empty report from TVar" $ do
    base <- baseReport >>= readTVarIO
    base `shouldBe` R.ReportData 0 0 0
  it "STM: Modifies reports (when duplicate)" $ do
    base <- baseReport
    atomically $ R.reportOnDuplicate base
    modified <- readTVarIO base
    modified `shouldBe` R.ReportData 0 1 0
  it "STM: Modifies reports (when unique)" $ do
    base <- baseReport
    atomically $ R.reportOnNew base
    modified <- readTVarIO base
    modified `shouldBe` R.ReportData 1 0 1
  it "STM: Modifies reports (when new and duplicate)" $ do
    base <- baseReport
    atomically $ R.reportOnNew base
    atomically $ R.reportOnDuplicate base
    modified <- readTVarIO base
    modified `shouldBe` R.ReportData 1 1 1
  it "STM: Modifies reports (resets)" $ do
    base <- baseReport
    atomically $ R.reportOnNew base
    atomically $ R.reportOnDuplicate base
    atomically $ R.reportRunReset base
    modified <- readTVarIO base
    modified `shouldBe` R.ReportData 0 0 1

--   -- I.parseMaybeInput "terminate\n" `shouldBe` Just I.Terminate
--   -- I.parseMaybeInput "terminate\n\r" `shouldBe` Just I.Terminate
--   -- I.parseMaybeInput "terminate\r\n" `shouldBe` Just I.Terminate
--   -- I.parseMaybeInput "terminate\r" `shouldBe` Just I.Terminate
-- it "Fails with 'terminate' and spaces intercalated" $ pure ()
--   -- I.parseMaybeInput " terminate" `shouldBe` Nothing
--   -- I.parseMaybeInput "terminate \n" `shouldBe` Nothing
--   -- I.parseMaybeInput " terminate " `shouldBe` Nothing
--   -- I.parseMaybeInput " terminate\r" `shouldBe` Nothing
--   -- I.parseMaybeInput "term inate " `shouldBe` Nothing
-- I.parseMaybeInput " terminate " `shouldBe` Nothing

baseReport :: IO R.Report
baseReport = R.createReport