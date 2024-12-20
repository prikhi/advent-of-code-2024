module Data.Set (
    Set,
    empty,
    fromList,
    toList,
    member,
    size,
    insert,
) where

import Data.Maybe (isJust)

import Data.Map qualified as M


-- | Uniqueness Sets via Binary Search Trees.
newtype Set a = Set
    { fromSet :: M.Map a ()
    }
    deriving (Show, Read, Eq)


instance Functor Set where
    fmap f = Set . M.mapKeys f . fromSet


instance Foldable Set where
    foldMap f =
        foldMap f . toList


-- | The null set
empty :: Set a
empty = Set M.empty


-- | List to Set conversion
fromList :: (Ord a) => [a] -> Set a
fromList xs = Set . M.fromList . zip xs $ repeat ()


-- | Set to List conversion
toList :: Set a -> [a]
toList = map fst . M.toList . fromSet


-- | Insert an element into the Set
insert :: (Ord a) => a -> Set a -> Set a
insert k = Set . M.insert k () . fromSet


-- | Does the Set contain the given element?
member :: (Ord a) => a -> Set a -> Bool
member k = isJust . M.lookup k . fromSet


-- | Count the elements in a Set
size :: Set a -> Int
size = M.size . fromSet
