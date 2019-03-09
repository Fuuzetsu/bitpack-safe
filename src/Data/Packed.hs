{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -O2 -ddump-simpl -ddump-to-file -dsuppress-coercions -ddump-asm #-}
{-# OPTIONS_GHC -Wno-implicit-prelude #-}

-- |
-- Module      : Data.Packed
-- Description : Type-safe packing and extraction of bit collections.
-- Copyright   : (c) Mateusz Kowalczyk, 2019
-- License     : BSD3
-- Maintainer  : fuuzetsu@fuuzetsu.co.uk
-- Stability   : experimental
-- Portability : POSIX
--
-- See function descriptions for documentation. You may want to start
-- at 'pack'.
module Data.Packed
    ( AllFields
    , Container
    , Decoder(..)
    , DecoderType(..)
    , Packable(..)
    , Packed
    , Pad
    , Raw
    , fieldsAre
    , fieldsSet
    , pack
    , unpackField
    , withFields
    -- * Unsafe primitives
    , extractPacked
    , unsafePack
    ) where

import Data.Bits ((.&.), complement, unsafeShiftL, unsafeShiftR)
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy(Proxy))
import GHC.TypeLits
   ( TypeError, ErrorMessage(Text, ShowType, (:<>:)), KnownNat, Nat, natVal
   , type (^), type (-), type (*), type (-), type (+), type CmpNat
   )
import GHC.Word (Word8, Word16, Word32, Word64)

-- | The underlying container. Currently it's 'Word64' and it's not
-- possible to swap it out.
--
-- In future versions, this will likely be user-specifiable.
type Container = Word64

-- | A 'Container' tagged with the list of fields that it contains.
--
-- Use 'pack' to construct it.
newtype Packed (a :: [Type]) = Packed Container

-- | A generic escape hatch. It's only useful if for example you're
-- going to be serialising the value as-is.
extractPacked :: Packed a -> Container
extractPacked (Packed a) = a

-- | A very unsafe 'Packed' value creation. Unless you got the
-- 'Container' from 'extractPacked' of the exact same type, all bets
-- are off. It may be useful if you're serialising the 'Packed' and
-- need to read it back _and_ you haven't changed the type.
unsafePack :: Container -> Packed a
unsafePack = Packed

-- | Underlying value that's in its original position. If we're only
-- doing something simple such as comparing tags, it's cheaper (indeed
-- for constants: usually free, at compile time) to prepare the value
-- in the "correct" position to start with then simply compare the two
-- numbers. For example, checking if 63rd bit is 1 can be done in two
-- ways: you can mask 63rd bit, shift 63 places to the right and check
-- if result is 1 _or_ you can mask 63rd bit and compare it to 1
-- shifted 63 places left. We know the mask at compile time but more
-- importantly, we also know (1 << 63) at compile time. In former
-- approach, we pay for a shift and comparison why in the latter
-- approach we can pay for a comparison of the correct location only.
--
-- For simpler but less efficient approach, see 'Clean'.
newtype Raw a = Raw Container
    deriving Eq

-- | Decoding modes.
data DecoderType = CleanDecoder | RawDecoder

data Decoder a d where
    -- | Underlying value shifted into lowest bits. This means that you
    -- can use this as-is: if you encoded something with 'packOne' as @1@,
    -- at decoding time you should be able to find @1@ in the 'Clean'.
    --
    -- When decoding with this method, you should _not_ assume that
    -- all but the 'PackedSized' lowest bits are cleared!
    --
    -- This approach can cost additional shift(s): see 'RawD' for
    -- an alternative.
    CleanD :: (Container -> Unpack a) -> Decoder a 'CleanDecoder
    RawD :: ((Container -> Raw a) -> Raw a -> Unpack a) -> Decoder a 'RawDecoder

-- | A class which describes how to pack and unpack something. It is
-- mostly governed by type families:
--
-- ['PackedSize']: The number of bits that this value uses.
--
-- ['Unpack']: What does the element decode to? In most cases (and a
-- default) is that the element decodes to itself. This type family
-- may be useful if you are encoding some additional information on
-- type level that you are using when packing/unpacking but wish the
-- result to be "simpler".
--
-- ['DecoderTyp']: Decoding mode. See 'Decoder'.
class Packable (a :: Type) where
    type PackedSize a :: Nat
    type Unpack a :: Type
    type Unpack a = a
    type DecoderTyp a :: DecoderType
    -- | Pack an element into the lowest bits of an 'Container'. Only
    -- 'PackedSize' bits will be subsequently used. Any other bits
    -- will be discarded. You are allowed to set all the bits but only
    -- the lowest 'PackedSize' bits will be used.
    --
    -- It is usually not recommended to use functions based on
    -- GHC-derived 'fromEnum' if your type has few constructors. See
    --
    -- https://ghc.haskell.org/trac/ghc/ticket/16364
    packOne :: a -> Container
    -- | Unpack an element. See 'DecoderTyp' for various modes of
    -- access to the bits.
    unpackOne :: Decoder a (DecoderTyp a)

instance Packable Word8 where
    type PackedSize Word8 = 8
    type DecoderTyp Word8 = 'CleanDecoder
    {-# INLINE packOne #-}
    packOne = fromIntegral
    {-# INLINE unpackOne #-}
    unpackOne = CleanD fromIntegral

instance Packable Word16 where
    type PackedSize Word16 = 16
    type DecoderTyp Word16 = 'CleanDecoder
    {-# INLINE packOne #-}
    packOne = fromIntegral
    {-# INLINE unpackOne #-}
    unpackOne = CleanD fromIntegral

instance Packable Word32 where
    type PackedSize Word32 = 32
    type DecoderTyp Word32 = 'CleanDecoder
    {-# INLINE packOne #-}
    packOne = fromIntegral
    {-# INLINE unpackOne #-}
    unpackOne = CleanD fromIntegral

instance Packable Word64 where
    type PackedSize Word64 = 64
    type DecoderTyp Word64 = 'CleanDecoder
    {-# INLINE packOne #-}
    packOne = id
    {-# INLINE unpackOne #-}
    unpackOne = CleanD id

instance Packable Int where
    type PackedSize Int = 64
    type DecoderTyp Int = 'CleanDecoder
    {-# INLINE packOne #-}
    packOne = fromIntegral
    {-# INLINE unpackOne #-}
    unpackOne = CleanD fromIntegral

instance Packable Bool where
    type PackedSize Bool = 1
    type DecoderTyp Bool = 'RawDecoder
    {-# INLINE packOne #-}
    packOne True = 1
    packOne False = 0
    {-# INLINE unpackOne #-}
    unpackOne = RawD $ \toRaw raw -> toRaw 0 /= raw

-- | An empty type with nothing stored. Useful if you don't have
-- enough data to fill the container completely or want to have gaps
-- in the middle for the future.
data Pad (x :: Nat)

instance Packable (Pad n) where
    type PackedSize (Pad n) = n
    type DecoderTyp (Pad n) = 'CleanDecoder
    {-# INLINE packOne #-}
    packOne _ = 0
    {-# INLINE unpackOne #-}
    unpackOne = error "unpackOne: Tried to force Pad thunk"

-- | Filters any 'Pad' it can spot. Useful for skipping 'Pad' in
-- function and fields.
type family FilterPad xs where
    FilterPad '[] = '[]
    FilterPad (S _ _ (Pad _) ': xs) = FilterPad xs
    FilterPad (Pad _ ': xs) = FilterPad xs
    FilterPad (x ': xs) = x : FilterPad xs

-- | There are scenarioes in which applying the a mask is not
-- necessary to extract the underlying value as there is a better way
-- to do so.
--
-- * p = our packed value, opaque
-- * positionMask = 0x0000ffff00000000
-- * ofs = 32
--
-- The standard way to proceed is to apply the mask ('.&.') and shift
-- the resulting value into the lowest bits: in this case, by 32
-- places.
--
-- There are special cases however! There exist operations which
-- directly work on the lowest N bits, where N = 8, 16, 32. If our
-- mask is exactly the same size then there is no point to applying
-- it: we can simply do a shift and the extraction operation will
-- simply only use the lowest bits. In our case, we notice that the
-- mask is exactly 16 bits so we leave everything in tact and shift
-- them.
--
-- A clever compiler such as Clang can realise this automatically but
-- GHC can not, so we perform this change explicitly but at compile
-- time.
class FastMask t where
    -- | @runFastMask packedValue maskValue@
    runFastMask :: Container -> Container -> Container

-- | Checks if the underlying type we're working on is one of the
-- special cases.
type family FastExtract (n :: Nat) :: Bool where
    FastExtract 8 = 'True
    FastExtract 16 = 'True
    FastExtract 32 = 'True
    FastExtract 64  = 'True
    FastExtract _ = 'False

-- | The reader will automatically use the correct number of bits so
-- do nothing.
instance FastMask 'True where
    {-# INLINE runFastMask #-}
    runFastMask i _ = i

-- | We have a non-nice mask so make sure to extract only the bits we
-- care about.
instance FastMask 'False where
    {-# INLINE runFastMask #-}
    runFastMask p mask = p .&. mask

-- | Dispatch any type to its fast mask implementation based on its
-- 'PackedSize'.
instance FastMask (FastExtract (PackedSize t)) => FastMask (t :: Type) where
    {-# INLINE runFastMask #-}
    runFastMask = runFastMask @(FastExtract (PackedSize t))

-- | Helper for type-level bitmask creations.
type BitMask ofs t =
    ( -- We know how to pack the underlying in place.
      Packable t
      -- We know where it has to live.
    , KnownNat ofs
      -- We know how to make the bitmask for its position.
    , KnownNat (PositionMask ofs t)
    )

instance (FastMask (S i ofs t), BitMask ofs t) => Packable (S i ofs t) where
    type PackedSize (S i ofs t) = PackedSize t
    type Unpack (S i ofs t) = Unpack t
    type DecoderTyp (S i ofs t) = 'CleanDecoder
    {-# INLINE packOne #-}
    packOne (S t) = packOne t `unsafeShiftL` fromInteger (natVal @ofs Proxy)
    -- The INLINE here is absolutely essential.
    {-# INLINE unpackOne #-}
    unpackOne = CleanD $ \p -> case unpackOne @t of
        -- This clean branch can be more efficient in some cases:
        -- clang can detect it for example.
        CleanD f -> f $ runFastMask @(S i ofs t) p positionMask `unsafeShiftR` ofs
        RawD f -> f (\x -> Raw (x `unsafeShiftL` ofs)) (Raw (p .&. positionMask))
        where
            Mask positionMask = makeMask @i @ofs @t
            ofs :: Int
            ofs = fromInteger (natVal @ofs Proxy)

-- | Convert a simple list of fields into fields tagged with their
-- offset. This ensures that we nothing overlaps and that we have the
-- exact number of bits filled in that the 'Container' holds.
type family ToShiftedOffsets ix xs left used where
    ToShiftedOffsets _ '[] 0 64 = '[]
    ToShiftedOffsets _ '[] _left used = TypeError
        ('Text "You need to use all 64 bits but you tried to use "
         ':<>: 'ShowType used
         ':<>: 'Text ".")
    ToShiftedOffsets ix (x ': xs) left used =
        S ix (left - PackedSize x) x
        ': ToShiftedOffsets (ix + 1) xs (left - PackedSize x) (used + PackedSize x)

-- | Calculate explicit 'S' offsets for every field. Ensures we fill
-- the whole 'Container' exactly. Doing it this way also ensures we
-- can't have overlaps.
type family ToShifted xs where
    ToShifted xs = ToShiftedOffsets 0 xs 64 0

-- | Value tagged with its offset from the lowest bit, i.e. where the
-- bits of the value end. Also tagged with the index of the field it
-- belong to for type errors. The index is kind-polymorphic but the
-- library itself only uses Nat for now.
--
-- Fairly arbitrary decision, other schemes would work too (such as
-- tagging top bit) but this works fine for calculations. Internal
-- only anyway so can be changed easily.
newtype S (ix :: k) (n :: Nat) t = S t

type family Field n xs where
    Field 0 (x : _) = x
    Field n (_ : xs) = Field (n - 1) xs

type family Fields' (ns :: [Nat]) (xs :: [Type]) :: [Type] where
    Fields' '[] _ = '[]
    Fields' (n ': ns) xs = Field n xs : Fields' ns xs

-- | Generates shift values for the fields then extracts the fields
-- with the given indices. It's done this way such that even if we,
-- for example, filter some 'Pad' fields, we still already have
-- generated the offsets and know where things live.
type family Fields ns xs where
    Fields ns xs = Fields' ns (ToShifted xs)

-- | Type-tagged 'Container' containing a mask or a joined collection
-- thereof. See 'JoinMasks' for an example.
newtype Mask (x :: k) = Mask Container

-- | Set @'PackedSize' n@ bits and shift them left @x@ places.
type family PositionMask (n :: Nat) (x :: Type) :: Nat where
    PositionMask n x = (2 ^ PackedSize x - 1) * (2 ^ n)

-- | Conjure a runtime value from a 'PositionMask'. This runtime value
-- usually gets folded away with others, see 'JoinMasks' for an
-- example.
makeMask :: forall i ofs x. KnownNat (PositionMask ofs x) => Mask (S i ofs x)
makeMask = Mask (fromInteger (natVal @(PositionMask ofs x) Proxy))

-- | Extract a single field from the packed collection. For multiple
-- fields, see 'withFields'.
--
-- @
-- type ThreeBoolsWord16 = Packed '[Bool, Bool, Bool, Pad 29, Word32]
--
-- unpackFirstBool :: ThreeBoolsWord16 -> Bool
-- unpackFirstBool = unpackField @0
-- @
unpackField
    :: forall i xs ofs t.
       ( Packable (Field i (ToShifted xs))
       , Field i (ToShifted xs) ~ S i ofs t
       )
    => Packed xs -> Unpack (S i ofs t)
unpackField (Packed x) = case unpackOne @(S i ofs t) of
    CleanD f -> f x

-- | Enforce disjoint fields.
--
-- While at the current state the code is written in such a way that
-- GHC has as much information as it needs at compile time and it does
-- a great job with it, we don't always have the luxury of knowing
-- values at compile time and so may end up producing functions with
-- chained function calls. If these functions are .|., GHC is not very
-- good at re-arranging these to constant fold the parts it does know
-- about. See
--
-- https://ghc.haskell.org/trac/ghc/ticket/16351
--
-- However, this library has an advantage that we can use. We are very
-- careful to ensure that all the fields do not overlap in the
-- container. Also notice that "x & y == 0" implies "x | y == x + y".
--
-- The 'Disjoint' type family throws a type error if we try to use the
-- same field twice. Once we know that we don't have the same field
-- twice, we know that we have no overlapping fields and we can use
-- '+' instead of '.|.' which GHC is able to optimise a lot better.
class DisjointOr n ns where
    disjointOr :: Container -> Container -> Container

instance ( Disjoint (S ix ofs x) xs
         ) => DisjointOr (S ix ofs x) xs where
    {-# INLINE disjointOr #-}
    disjointOr = (+)

-- | All our masks are non-overlapping which means that when we want
-- to extract multiple fields at once for a simple comparison, we can
-- actually just join the compile-time-known masks together first and
-- only do a single comparison later.
class JoinMasks (a :: [Type]) where
    joinMasks :: Mask a

instance JoinMasks '[] where
    {-# INLINE joinMasks #-}
    joinMasks = Mask 0

instance ( KnownNat (PositionMask n x)
         , Disjoint (S i n x) xs
         , JoinMasks xs
         ) => JoinMasks (S i n x ': xs) where
    {-# INLINE joinMasks #-}
    joinMasks = case makeMask @i @n @x of
        Mask m -> case joinMasks @xs of
            Mask n -> Mask (disjointOr @(S i n x) @xs m n)

-- | Collapse runtime masks with variadic function, internal.
-- Hopefully makes GHC spirit everything known away.
class AskPack xs (a :: Type) where
    askPack :: Container -> Container -> a

instance AskPack '[] Bool where
    {-# INLINE askPack #-}
    askPack acc p = acc == p

-- | For this instance, the @p@ is already '.&.'d with @acc@s mask
-- complement, that is the two are disjoint.
--
-- See 'DisjointOr' for why this matters to us.
instance AskPack '[] (Packed xs) where
    {-# INLINE askPack #-}
    askPack acc p = Packed (acc + p)

instance ( AskPack as r
         , BitMask ofs a
         , Disjoint (S i ofs a) as
         , FastMask (S i ofs a)
         ) => AskPack (S i ofs a : as) (a -> r) where
    {-# INLINE askPack #-}
    askPack acc p a = askPack @as
        (disjointOr @(S i ofs a) @as acc (packOne @(S i ofs a) (S a))) p

type family AskFor ns xs t where
    AskFor '[n] xs t = Field n xs -> t
    AskFor (n ': ns) xs t = Field n xs -> AskFor ns xs t
    AskFor '[] _ _ = TypeError
      ('Text "You need to ask for at least a single field.")

type family Disjoint x ys :: Constraint where
    Disjoint _ '[] = ()
    Disjoint (S ix ofsx x) (S iy _ _ ': ys) =
        (ErrEq ix (CmpNat ix iy), Disjoint (S ix ofsx x) ys)

type family ErrEq n o :: Constraint where
    ErrEq n 'EQ = TypeError
        ( 'Text "Tried acessing the field "
          ':<>: 'ShowType n
          ':<>: 'Text " more than once."
        )
    ErrEq _ _ = ()

-- | Check if multiple fields are set with the given values. It works
-- by doing an equality check on packed representations.
--
-- @
-- type ThreeBoolsWord16 = Packed '[Bool, Bool, Bool, Pad 29, Word32]
--
-- -- Check if first Bool is True, third Bool is False and the Word32 is 42
-- f :: ThreeBoolsWord16
-- f i = fieldsAre @[0, 2, 4] i True False 42
-- @
--
-- If the values you're checking with are known at compile time, this
-- can easily reduce to efficient code. At the time of writing, the
-- above example produces:
--
-- @
-- f = \ (i_a3zb :: ThreeBoolsWord16) ->
--       case i_a3zb `cast` <Co:25> of { W64# x#_i4jl ->
--       case ghc-prim-0.5.3:GHC.Prim.and# x#_i4jl 0xa0000000ffffffff## of {
--         __DEFAULT -> ghc-prim-0.5.3:GHC.Types.False;
--         0x800000000000002a## -> ghc-prim-0.5.3:GHC.Types.True
--       }
--       }
-- @
{-# INLINE fieldsAre #-}
fieldsAre
    :: forall ns xs.
       ( AskPack (Fields ns xs) (AskFor ns xs Bool)
       , JoinMasks (Fields ns xs)
       )
    => Packed xs -> AskFor ns xs Bool
fieldsAre (Packed x) = askPack @(Fields ns xs) 0 (x .&. positionMask)
    where
        Mask positionMask = joinMasks @(Fields ns xs)

-- | Set the given set of fields.
--
-- Just like 'fieldsAre', this should generate efficient code. If the
-- fields are known, it can simply set everything at once with very
-- little effort from the user.
--
-- @
-- type ThreeBoolsWord16 = Packed '[Bool, Bool, Bool, Pad 29, Word32]
--
-- setTrue123 :: ThreeBoolsWord16 -> ThreeBoolsWord16
-- setTrue123 i = fieldsSet @[1, 4] i True 123
-- @
--
-- Can be expected to generate something like
--
-- @
-- Data.Packed.Example.setTrue1
--   = \ (i_a3ye :: ThreeBoolsWord16) ->
--       case i_a3ye `cast` <Co:25> of { W64# x#_i4jH ->
--       GHC.Word.W64#
--         (ghc-prim-0.5.3:GHC.Prim.or#
--            0x400000000000007b##
--            (ghc-prim-0.5.3:GHC.Prim.and# x#_i4jH 0xbfffffff00000000##))
--       }
-- @
--
-- TODO: code above is maybe not the best actually?
{-# INLINE fieldsSet #-}
fieldsSet
    :: forall ns xs.
       ( AskPack (Fields ns xs) (AskFor ns xs (Packed xs))
       , JoinMasks (Fields ns xs)
       )
    => Packed xs -> AskFor ns xs (Packed xs)
fieldsSet (Packed x) = askPack @(Fields ns xs) 0
    -- Take a mask that keeps everything but the values we want to set
    -- then in 'AskPack' we set only those positions with an OR,
    -- ending up with exact updated set of fields.
    (x .&. complement positionMask)
    where
        Mask positionMask = joinMasks @(Fields ns xs)

-- | Applies the given function to the unpacked set of fields that was requested.
class WithFields xs f t where
    withFields' :: Packed xs -> f -> t

instance WithFields '[] t t where
    {-# INLINE withFields' #-}
    withFields' _ t = t

instance ( FastMask (S i ofs x)
           -- We can make the bitmask  for the current argument.
         , BitMask ofs x
           -- We can use the unpacked, unshifted argument.
         , Unpack (S i ofs x) ~ x
           -- We know how to unpack next argument to come.
         , WithFields xs b t
         ) => WithFields (S i ofs x ': xs) (x -> b) t where
    {-# INLINE withFields' #-}
    withFields' (Packed p) f = case unpackOne @(S i ofs x) of
        CleanD u -> withFields' @xs (Packed p) (f (u p))

-- | Apply the given function over the specified fields.
--
-- This can be used, for example, to unpack the structure into
-- something.
--
-- @
-- type ThreeBoolsWord16 = Packed '[Bool, Bool, Bool, Pad 29, Word32]
--
-- unpackTBW16 :: ThreeBoolsWord16 -> (Bool, Word32)
-- unpackTBW16 i = withFields @[0, 4] i (,)
-- @
--
-- See 'unpackField' if you only want a single field.
{-# INLINE withFields #-}
withFields
    :: forall ns xs t. WithFields (Fields ns xs) (AskFor ns xs t) t
    => Packed xs -> AskFor ns xs t -> t
withFields (Packed p) = withFields' @(Fields ns xs) (Packed p)

-- | A clumsy way to extract indicies of all "relevant" fields, i.e.
-- skipping padding &c.
--
-- @
-- withFields @(AllFields Foo) …
-- @
--
-- This can ensure we're not forgetting anything if we happen to
-- change Foo, such as by replacing part of Pad with an actual field.
type family AllFields (xs :: k) :: [Nat] where
    AllFields (Packed xs) = FieldIxs (FilterPad (Fields (AllFieldsHelper 0 xs) xs))

type family FieldIxs xs :: [Nat] where
    FieldIxs '[] = '[]
    FieldIxs (S n _ _ ': xs) = n ': FieldIxs xs

type family AllFieldsHelper n xs :: [Nat] where
    AllFieldsHelper _ '[] = '[]
    AllFieldsHelper n (_ ': xs) = n : AllFieldsHelper (n + 1) xs

-- | Extract the field list from 'Packed'. Helper only.
type family Underlying x where
    Underlying (Packed x) = x

class Pack ts xs (a :: Type) where
    packC :: Container -> a

instance Pack ts '[] (Packed ts) where
    {-# INLINE packC #-}
    packC = Packed

instance ( BitMask ofs a
         , FastMask (S i ofs a)
         , Disjoint (S i ofs a) as
         , Pack ts as r
         ) => Pack ts (S i ofs a ': as) (a -> r) where
    -- We are safe to use + here as the user can't construct
    -- overlapping fields in this method. See
    -- [Note Addition instead of bitwise or]
    {-# INLINE packC #-}
    packC acc a = packC @ts @as
        (disjointOr @(S i ofs a) @as acc (packOne @(S i ofs a) (S a)))

type family RunPack (xs :: Type) where
    RunPack (Packed xs) = RunPackHelper (FilterPad xs) (Packed xs)

type family RunPackHelper (xs :: [Type]) ys where
    RunPackHelper '[] ys = ys
    RunPackHelper (x ': xs) ys = x -> RunPackHelper xs ys

-- | Pack values into the specified type. 'Pad' fields are skipped.
--
-- It is a type error to try to pack a @'Packed' xs@ where @xs@ does
-- not exactly fill the underlying 'Container'.
--
-- @
-- type ThreeBoolsWord16 = Packed '[Bool, Bool, Bool, Pad 29, Word32]
--
-- tbw16 :: ThreeBoolsWord16
-- tbw16 = pack @ThreeBoolsWord16 True False False 42
-- @
{-# INLINE pack #-}
pack
    :: forall xs.
       Pack (Underlying xs) (FilterPad (ToShifted (Underlying xs))) (RunPack xs)
    => RunPack xs
pack = packC @(Underlying xs) @(FilterPad (ToShifted (Underlying xs))) 0
