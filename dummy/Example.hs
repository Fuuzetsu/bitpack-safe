{-# OPTIONS_GHC -O2 -ddump-simpl -ddump-to-file -dsuppress-coercions -ddump-asm -dhex-word-literals -ddump-stg #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Data.Packed.Example where

import GHC.Word
import Prelude
import Data.Packed

data InstType = Opt | Fut | Spr
    deriving (Show, Eq)
instance Packable InstType where
    type PackedSize InstType = 2
    type DecoderTyp InstType = 'RawDecoder
    {-# INLINE packOne #-}
    packOne Opt = 0
    packOne Fut = 1
    packOne Spr = 2
    {-# INLINE unpackOne #-}
    unpackOne = RawD $ \toRaw raw -> if
        | toRaw 0 == raw -> Opt
        | toRaw 1 == raw -> Fut
        | otherwise -> Spr

data Dir = Bid | Offer
    deriving Show
instance Packable Dir where
    type PackedSize Dir = 1
    type DecoderTyp Dir = 'RawDecoder
    {-# INLINE packOne #-}
    packOne Bid = 0
    packOne Offer = 1
    {-# INLINE unpackOne #-}
    unpackOne = RawD $ \toRaw raw ->
        if toRaw 0 == raw then Bid else Offer

type Qty = Word16
type Price = Word16

data InstView =
    Option !Dir !Qty !Price
    | Future !Dir !Qty !Price
    | Spread !Dir !Price
    deriving Show

type Inst = Packed
    '[ InstType
     , Pad 13
     , Dir
     , Qty
     , Pad 16
     , Price
     ]

{-# INLINE packInst #-}
packInst :: InstView -> Inst
packInst (Option d q p) = pack @Inst Opt d q p
packInst (Future d q p) = pack @Inst Fut d q p
packInst (Spread d p)   = pack @Inst Spr d 0 p

{-# INLINE unpackInst #-}
unpackInst :: Inst -> InstView
unpackInst i = case unpackField @0 i of
    Opt -> withFields @[2, 3, 5] i Option
    Fut -> withFields @[2, 3, 5] i Future
    Spr -> withFields @[2, 5] i Spread

inst :: Inst
inst = packInst (Future Offer 7 3)

isBidFut2 :: Inst -> Bool
isBidFut2 i = fieldsAre @[0, 2] i Fut Bid

isBidFut3 :: Inst -> Bool
isBidFut3 i = case unpackInst i of
    Future Bid _ _ -> True
    _ -> False

foo :: Bool
foo = isBidFut2 inst

bar :: InstView
bar = unpackInst inst

type ThreeBoolsWord16 = Packed '[Bool, Bool, Bool, Pad 29, Word32]

tbw16 :: ThreeBoolsWord16
tbw16 = pack @ThreeBoolsWord16 True False False 42

unpackFirstBool :: ThreeBoolsWord16 -> Bool
unpackFirstBool = unpackField @0

unp :: ThreeBoolsWord16 -> (Bool, Bool, Bool, Word32)
unp i = withFields @[0,1,2,4] i (,,,)

setTrue123 :: ThreeBoolsWord16 -> ThreeBoolsWord16
setTrue123 i = fieldsSet @[1, 4] i True 123

-- Check if first Bool is True, third Bool is False and the Word32 is 42
f ::  ThreeBoolsWord16 -> Bool
f i = fieldsAre @[0, 2, 4] i True False 42

res :: Bool
res = f tbw16
