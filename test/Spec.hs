{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Main (main) where

import Data.Packed
import Data.Word (Word32, Word16)
import Test.QuickCheck.Gen (elements)
import Test.QuickCheck.Arbitrary (Arbitrary(arbitrary))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)

data Three = Zero | One | Two
  deriving (Show, Eq)

instance Packable Three where
  type PackedSize Three = 4
  type DecoderTyp Three = 'RawDecoder
  {-# INLINE packOne #-}
  packOne Zero = 0
  packOne One = 1
  packOne Two = 2
  {-# INLINE unpackOne #-}
  unpackOne = RawD $ \toRaw u -> if
    | toRaw 0 == u -> Zero
    | toRaw 1 == u -> One
    | otherwise -> Two

instance Arbitrary Three where
  arbitrary = elements [Zero, One, Two]

type Test = Packed '[Word32, Three, Bool, Pad 9, Word16, Pad 2]

packTest :: (Word32, Three, Bool, Word16) -> Test
packTest (w1, w2, w3, w4) = pack @Test w1 w2 w3 w4

unpackTest :: Test -> (Word32, Three, Bool, Word16)
unpackTest p = withFields @(AllFields Test) p (,,,)

main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Tests"
  [ testProperty "unpack . pack ~ id" $ \inp ->
      unpackTest (packTest inp) == inp
  , testProperty "pack x ~ fieldsAre @all x == True" $ \inp -> case inp of
      (x1, x2, x3, x4) -> fieldsAre @(AllFields Test) (packTest inp) x1 x2 x3 x4
  , testProperty "setFields xs (pack x) -> fieldsAre @all xs == True" $ \inp1 inp2 ->
      case inp2 of
        (y1, y2, y3, y4) ->
          let t = fieldsSet @(AllFields Test) (packTest inp1) y1 y2 y3 y4
          in fieldsAre @(AllFields Test) t y1 y2 y3 y4
  ]
