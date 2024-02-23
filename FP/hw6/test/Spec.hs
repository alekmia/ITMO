{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}

import Test.Hspec

import HW6.T1
import HW6.T2

data Proxy a = Proxy
    deriving (Show, Eq)

type ExampleSet = '[ "a", "b" ] :: TSet

main :: IO ()
main = hspec $ do
    describe "HW6 test" $ do
        it "T1 test" $ do
            cnt <- newCHT
            putCHT "2" (7 :: Int) cnt
            size <- sizeCHT cnt
            size `shouldBe` 1
            val <- getCHT "2" cnt
            val `shouldBe` Just 7
            --
            putCHT "3" (2 :: Int) cnt
            size2 <- sizeCHT cnt
            size2 `shouldBe` 2
            val2 <- getCHT "3" cnt
            val2 `shouldBe` Just 2
            --
            putCHT "2" (14 :: Int) cnt
            size3 <- sizeCHT cnt
            size3 `shouldBe` 2
            val3 <- getCHT "2" cnt
            val3 `shouldBe` Just 14
        it "T2 test" $ do
            Proxy @(Contains "a" ExampleSet) `shouldBe` Proxy @'True
            Proxy @(Contains "c" ExampleSet) `shouldBe` Proxy @'False
            Proxy @(Delete "a" ExampleSet) `shouldBe` Proxy @'[ "b" ]
            Proxy @(Add "14" ExampleSet) `shouldBe` Proxy @'[ "14", "a", "b" ]


