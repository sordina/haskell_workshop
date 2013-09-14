{-# LANGUAGE    TupleSections #-}
{-# LANGUAGE    FlexibleInstances #-}
{-# LANGUAGE    ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.Compression.Simple
( packString
, unpackString
, decompress
, frequenciesToCoding
, tests
)

where

-- Imports
--
import Data.Bits          (setBit, testBit)
import Data.Word8         (Word8)
import Data.Maybe         (mapMaybe, fromJust)
import Data.List.Split    (chunksOf)
import Data.Monoid        (Monoid, mappend, mempty, (<>), Sum(Sum))
import Control.Arrow      (first, second, (&&&))
import Data.List          (nub, isPrefixOf, delete, stripPrefix, group, sortBy, find, sort)
import Data.Ord           (comparing)

import qualified Data.ByteString as BS

import Test.QuickCheck as Q

-- Types
--
type Lookup k v       = [(k,v)]
type BitString        = [ Bool ]
type CharBits         = Lookup Char BitString
type BitsChar         = Lookup BitString Char
type OccuranceTree v  = FingerTree (Sum Int) v
type FrequencyTable v = Lookup Int v
type NonEmptyString   = Q.NonEmptyList Char

-- Data
--
data FingerTree k v = L k v | B k (FingerTree k v) (FingerTree k v)
  deriving Show

-- Instances
--
instance Monoid k => Monoid (FingerTree k v)
  where x `mappend` y = B (getKey x `mappend` getKey y) x y
        mempty        = error "We should not need this" -- Our data is really a semigroup, but Monoids are nicer to work with

instance Arbitrary v => Arbitrary (OccuranceTree v)
  where arbitrary = fmap (L (Sum 1)) arbitrary

instance Testable (Maybe Bool) where
  property Nothing  = property True
  property (Just x) = property x

-- Bits and Bytes
--
toByteString :: BitString -> BS.ByteString
toByteString = BS.pack . map toWord . chunksOf 8

toWord :: BitString -> Word8
toWord bs = foldl update 0 (zip [0..] bs)
  where
    update b (i,True ) = setBit b i
    update b (_,False) = b

toBits :: BS.ByteString -> BitString
toBits = concatMap wordBits . BS.unpack
  where
    wordBits b = map (testBit b) [0..7]


-- Trees
--
treeSize :: FingerTree x y -> Int
treeSize (L   _ _) = 1
treeSize (B _ l r) = treeSize l + treeSize r

isLeaf :: FingerTree x y -> Bool
isLeaf (L _ _) = True
isLeaf _       = False

getKey :: FingerTree t t1 -> t
getKey (L   k _  ) = k
getKey (B k _ _) = k

step :: (Monoid k, Ord k) => [FingerTree k v] -> [FingerTree k v]
step []    = []
step [x]   = [x]
step [x,y] = [ x <> y ]
step xs    = step (take 2 sorted) ++ drop 2 sorted
  where
    sorted = sortBy (comparing getKey) xs

buildA :: (Monoid k, Ord k) => [FingerTree k v] -> [[FingerTree k v]]
buildA = iterate step

buildB :: (Monoid k, Ord k) => [(k,v)] -> Maybe [FingerTree k v]
buildB = find ((<= 1) . length) . buildA . map (uncurry L)

build :: (Monoid k, Ord k) => [(k,v)] -> FingerTree k v
build = head . fromJust . buildB

fromFrequencies :: [(Int, v)] -> OccuranceTree v
fromFrequencies = build . map (first Sum)

treeToBits :: OccuranceTree v -> Lookup v BitString
treeToBits t = result -- map (second reverse) result -- hint
  where
  result = if isLeaf t then coding [True] t -- Be productive for singleton trees
                       else coding []     t

coding :: BitString -> FingerTree t t1 -> Lookup t1 BitString
coding p (L _ v  ) = [ (v, p) ]
coding p (B _ l r) = coding (False : p) l ++ coding (True : p) r

frequenciesToCoding :: [(Int, v)] -> Lookup v BitString
frequenciesToCoding = treeToBits . fromFrequencies

-- Combination
--
packTable :: FrequencyTable Char -> String -> BS.ByteString
packTable t s = toByteString $ concat $ mapMaybe (flip lookup bitsLookup) s
  where
    bitsLookup = frequenciesToCoding t

packString :: String -> (FrequencyTable Char, BS.ByteString)
packString s = (t, packTable t s) where t = getTable s

unpackString :: CharBits -> BS.ByteString -> String
unpackString t = decompress flipped flipped . toBits
  where
  flipped = sortBy (comparing (length . fst)) (invert t)

decompress :: BitsChar -> BitsChar -> BitString -> String
decompress _   [] []     = []
decompress obc [] (_:xs) = decompress obc obc xs
decompress _ _ []        = []
decompress obc ((bs,c):t) s = case stripPrefix bs s
                                of Just r  -> c : decompress obc obc r
                                   Nothing ->     decompress obc t   s

-- Tools
--
noPrefixStarts :: Eq e => [[e]] -> Bool
noPrefixStarts xs = and [none (isPrefixOf x `map` delete x xs) | x <- xs]

invert :: [(a, b)] -> [(b, a)]
invert = map (snd &&& fst)

none :: [Bool] -> Bool
none = not . or

getTable :: String -> FrequencyTable Char
getTable = map (length &&& head) . group . sort

-- Properties
--
prop_toBits_bijection :: (Q.Positive Int) -> Property
prop_toBits_bijection (Q.getPositive -> n)
  = Q.forAll (Q.vector 128) $ \x ->
  let y = take (8*n) x in
  toBits (toByteString y) == y

prop_step_size :: [OccuranceTree Char] -> Bool
prop_step_size ls = sum (map treeSize ls) == sum (map treeSize (step ls))

prop_step_size2 :: [OccuranceTree Char] -> Bool
prop_step_size2 ls = sum (map treeSize ls) == sum (map treeSize (step (step ls)))

prop_buildB_size :: String -> Property
prop_buildB_size ls = not (null ls)
                 ==> Just [length ls] == fmap (fmap treeSize) (buildB ts)
  where
  ts = map (Sum (1 :: Int),) ls

prop_build_size :: NonEmptyString -> Bool
prop_build_size (Q.getNonEmpty -> ls) = (length ls) == treeSize (build ts)
  where
  ts = map (Sum (1 :: Int),) ls

prop_frequency_tree_size :: Q.NonEmptyList (Int, Char) -> Bool
prop_frequency_tree_size (Q.getNonEmpty -> ls) = length ls == treeSize (fromFrequencies ls)

prop_same_length :: Q.NonEmptyList (Q.Positive Int, Char) -> Bool
prop_same_length (map (first Q.getPositive) . Q.getNonEmpty -> ls)
  = length ls == length (frequenciesToCoding ls)

prop_contains_items :: Q.NonEmptyList (Q.Positive Int, Char) -> Bool
prop_contains_items (map (first Q.getPositive) . Q.getNonEmpty -> ls)
  = sort (map snd ls)
 == (sort . map fst) (frequenciesToCoding ls)

prop_roundtrip :: NonEmptyString -> Bool
prop_roundtrip (Q.getNonEmpty -> s) = s == take (length s) res
  where
    (t,bytes) = packString s
    bt        = frequenciesToCoding t
    res       = unpackString bt bytes

prop_no_prefix_starts :: Q.NonEmptyList (Q.Positive Int) -> NonEmptyString -> Bool
prop_no_prefix_starts (map Q.getPositive . Q.getNonEmpty -> ints)
                                          (Q.getNonEmpty -> chars)
                      = (noPrefixStarts . map snd) (frequenciesToCoding (zip ints (nub chars)))

prop_abc :: Bool
prop_abc = (noPrefixStarts . map snd) (frequenciesToCoding $ getTable "abc")

tests :: [(String, IO Result)]
tests = [ qc "prop_toBits_bijection   " prop_toBits_bijection
        , qc "prop_step_size          " prop_step_size
        , qc "prop_step_size2         " prop_step_size2
        , qc "prop_buildB_size        " prop_buildB_size
        , qc "prop_build_size         " prop_build_size
        , qc "prop_frequency_tree_size" prop_frequency_tree_size
        , qc "prop_same_length        " prop_same_length
        , qc "prop_contains_items     " prop_contains_items
        , qc "prop_roundtrip          " prop_roundtrip
        , qc "prop_no_prefix_starts   " prop_no_prefix_starts
        , qc "prop_abc                " prop_abc
        ]
        where
        qc :: Testable prop => t -> prop -> (t, IO Result)
        qc x y = (x, quickCheckResult y)
