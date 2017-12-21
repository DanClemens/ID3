-- | This module provides a very simple implementation of a decisiontree. It is \"optimized\" for readability, not so much for performance. I doubt it can be used for real (=huge) datasets, but it should be ok for a couple of hundred (thousand?) items.
-- 
-- You are encouraged to have a look at the source
-- 
-- It is build (for now) using the ID3 algorithm (or at least something closely resembling that). That means the attributes you choose must have a finite set of possible values.
module Data.DecisionTree (
    build,
    decide,
    Datum(D, dName, attributes),
    PreLabeled,
    Attribute(A, aName, possibleValues),
    DecisionTree) where

import Data.Maybe (fromJust)
import Data.List hiding (partition)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Function (on)

type PreLabeled a b= (b, Datum a)

-- | The type for our DecisionTree
data DecisionTree a b= Leaf b -- ^ Leafs have labels
    | Node { 
        att ::Attribute a, -- ^ a node asks for this attribute
        child :: a -> (DecisionTree a b) -- ^ and has children which can be found with a value of the attribute
        }

-- | A Datum has Attributes
data Attribute a = A {
    aName :: String, -- ^ Attributes have a name
    possibleValues ::  [a] -- ^ and a set of possible values
    } 

-- | Things we want to find labels for
data Datum a= D {
    dName :: String, -- ^ They have names
    attributes :: [(Attribute a,a)] -- ^ and attributes
    } deriving Show

instance (Show a, Show b) => Show (DecisionTree a b) where
    show x = showTree x ""

showTree :: (Show a, Show b) => DecisionTree a b -> ShowS
showTree (Leaf x) = shows x
showTree (Node att child) = ('<':).shows att.("|\n"++).showList [child a | a <- possibleValues att].('>':)

instance Eq (Attribute a) where
    (==) = (==) `on` aName
    
instance Show (Attribute a) where
    show = aName
    
-- | Build a DecisionTree from the given Trainingset
build :: (Ord a, Ord b) => [Attribute a] -> [PreLabeled a b] -> DecisionTree a b
build atts dataset =  case inf of
    0 -> Leaf dominantLabel -- even the best Attribute doesn't gain any information. We're done
    _ -> Node { 
        att = bAtt,
        child = safeLookup
        } 
    where
            (inf,bAtt) = bestAttribute dataset atts -- get the best attribute
            p =  partition dataset bAtt -- use it to partition the set
            children = Map.map (build atts) p -- recursivly build the children
            dominantLabel = fst $ Map.findMax $ groupLabels (map label dataset) -- in case we are done, get the label
            safeLookup a= fromJust $ Map.lookup a children
            
                 
-- | Which value does this Datum have for the given Attribute?
getValue :: Datum a-> Attribute a ->  a
getValue d att = fromJust $ lookup att (attributes d)

-- | Extract a label 
label :: PreLabeled a b -> b
label = fst

-- | Decide which label belongs to this Datum
decide :: Eq a => DecisionTree a b -> Datum a -> b
decide (Leaf b) _ = b -- we reached a Leaf, done
decide (Node att child) d = decide (child v) d where -- we're in a node, walk down
    v = getValue d att
    
-- | Partitions the Dataset according to the possible values of the attribute
partition :: (Ord a) =>[PreLabeled a b] -> Attribute a -> Map a [PreLabeled a b]
partition set att= foldl (\m k -> Map.insertWith (++) k [] m) grouped (possibleValues att)  where
    grouped = groupWith (flip getValue att.snd) (:[]) (++) set

-- | Computes the entropy of a Dataset
--
-- the Entropy is defined as: sum (p_i * log_2 p_i)
-- where p_i = |{ x | x has Label i}|/|Dataset|
entropy :: (Ord b) => [b] -> Double
entropy set= (-1)*(  Map.fold help 0 $ groupLabels set )
    where 
        n = fromIntegral $ length set
        help s acc | s/=0 = let p = fromIntegral s / n in acc+p*log p/log 2
        help _ _ = error "entropy: we are not supposed to get p=0"

-- we want to count how many Data we have for each label. Thus we group it with 1 as 
-- singleton value and add 1 whenever we find another Datum with the same label       
groupLabels :: Ord b => [b] -> Map b Int 
groupLabels = groupWith id (const (1::Int)) (const succ)

-- | How much information does this Attribute give us for the given Dataset
-- it is defined as 
--
-- entropy(set) - sum p_i * entropy {dat_i | dat has value i for attribute a}
information :: (Ord b, Ord a) =>  [PreLabeled a b] -- ^ the data
    -> Attribute a -- ^ the Attribute 
    -> Double -- ^ the Information
information dat att= entropy (map label dat) - sum (zipWith (*) pi (map entropy ps)) where
    ps = map (map label) $ Map.elems $ partition dat att -- the partitions, we're only interested in the labels
    pi = map ((/ n).fromIntegral.length) ps -- the size of the partition/size dataset
    n = fromIntegral $ length dat
    
-- | Return the attribute which gives us greatest gain in information
bestAttribute :: (Ord b, Ord a) => [PreLabeled a b] -> [Attribute a] -> (Double,Attribute a)
bestAttribute dat = head.sortBy (compare `on` negatedInf).computeInformation where
    negatedInf (inf,_) = -inf
    computeInformation = map (\x -> (information dat x,x))

-- | groups a Dataset using a Map. According to #haskell \"efficient\" grouping needs Ord. I agree with that
groupWith :: Ord k => (a -> k) -- ^ how to extract a key from a Datum
    -> (a -> v) -- ^ how to make a Datum into a value for the map
    -> (v -> v -> v) -- ^ how to fuse two values (should we have > 1 Data for this key)
    -> [a] -- ^ the list we want to group
    -> Map k v 
groupWith getKey singleton fuse = 
    foldl (\m x -> Map.insertWith fuse (getKey x) (singleton x) m) Map.empty
    

    
        