import Convolution
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  iacfTest
  cacfTest

iacfTest :: SpecWith ()
iacfTest = describe "Convolution" $ do
  it "returns the impulse autocorrelation function of *barker2*" $ do
    iacf barker2 barker2 `shouldBe` [-1, 2, -1]
  it "returns the impulse autocorrelation function of *barker2'*" $ do
    iacf barker2' barker2' `shouldBe` [1, 2, 1]
  it "returns the impulse autocorrelation function of *barker3*" $ do
    iacf barker3 barker3 `shouldBe` [-1, 0, 3, 0, -1]
  it "returns the impulse autocorrelation function of *barker4*" $ do
    iacf barker4 barker4 `shouldBe` [1, 0, -1, 4, -1, 0, 1]
  it "returns the impulse autocorrelation function of *barker4'*" $ do
    iacf barker4' barker4' `shouldBe` [-1, 0, 1, 4, 1, 0, -1]
  it "returns the impulse autocorrelation function of *barker5*" $ do
    iacf barker5 barker5 `shouldBe` [1, 0, 1, 0, 5, 0, 1, 0, 1]
  it "returns the impulse autocorrelation function of *barker7*" $ do
    iacf barker7 barker7 `shouldBe` [-1, 0, -1, 0, -1, 0, 7, 0, -1, 0, -1, 0, -1]
  it "returns the impulse autocorrelation function of *barker11*" $ do
    iacf barker11 barker11 `shouldBe` [-1, 0, -1, 0, -1, 0, -1, 0, -1, 0, 11, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1]
  it "returns the impulse autocorrelation function of *barker13*" $ do
    iacf barker13 barker13 `shouldBe` [1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 13, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1]

cacfTest :: SpecWith ()
cacfTest = describe "Convolution" $ do
  it "returns the circular autocorrelation function of *barker2*" $ do
    cacf barker2 barker2 `shouldBe` [2, -2]
  it "returns the circular autocorrelation function of *barker2'*" $ do
    cacf barker2' barker2' `shouldBe` [2, 2]
  it "returns the circular autocorrelation function of *barker3*" $ do
    cacf barker3 barker3 `shouldBe` [3, -1, -1]
  it "returns the circular autocorrelation function of *barker4*" $ do
    cacf barker4 barker4 `shouldBe` [4, 0, 0, 0]
  it "returns the circular autocorrelation function of *barker4'*" $ do
    cacf barker4' barker4' `shouldBe` [4, 0, 0, 0]
  it "returns the circular autocorrelation function of *barker5*" $ do
    cacf barker5 barker5 `shouldBe` [5, 1, 1, 1, 1]
  it "returns the circular autocorrelation function of *barker7*" $ do
    cacf barker7 barker7 `shouldBe` [7, -1, -1, -1, -1, -1, -1]
  it "returns the circular autocorrelation function of *barker11*" $ do
    cacf barker11 barker11 `shouldBe` [11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1]
  it "returns the circular autocorrelation function of *barker13*" $ do
    cacf barker13 barker13 `shouldBe` [13, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]

barker2 :: [Int]
barker2 = [1, -1]

barker2' :: [Int]
barker2' = [1, 1]

barker3 :: [Int]
barker3 = [1, 1, -1]

barker4 :: [Int]
barker4 = [1, -1, 1, 1]

barker4' :: [Int]
barker4' = [1, -1, -1, -1]

barker5 :: [Int]
barker5 = [1, 1, 1, -1, 1]

barker7 :: [Int]
barker7 = [1, 1, 1, -1, -1, 1, -1]

barker11 :: [Int]
barker11 = [1, 1, 1, -1, -1, -1, 1, -1, -1, 1, -1]

barker13 :: [Int]
barker13 = [1, 1, 1, 1, 1, -1, -1, 1, 1, -1, 1, -1, 1]
