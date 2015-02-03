{-# LANGUAGE OverloadedStrings #-}

module MediaTypeSpec (main, spec) where

import MediaType
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =

  describe "mediaType" $

    it "determines the correct MediaType of a FilePath" $ do

      mediaType "foo.png"  `shouldBe` Image
      mediaType "bar.md"   `shouldBe` Document
      mediaType "baz.flac" `shouldBe` Music
