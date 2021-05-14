module Data.IOHashMap.STM
  ( IOHashMap
  , newIOHashMap
  , readIOHashMap
  , modifyIOHashMap
  , empty
  , singleton
  , null
  , size
  , member
  , lookup
  , (!?)
  , findWithDefault
  , (!)
  , insert
  , insertWith
  , delete
  , adjust
  , update
  , alter
  , foldMapWithKey
  , foldr
  , foldl
  , foldr'
  , foldl'
  , foldrWithKey'
  , foldlWithKey'
  , foldrWithKey
  , foldlWithKey
  , keys
  , elems
  , toList
  , fromList
  ) where

import           Control.Concurrent.STM (STM, TVar, modifyTVar', newTVar,
                                         readTVar)
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HM
import           Data.Hashable
import           Prelude                hiding (foldl, foldr, lookup, null)

newtype IOHashMap k v = IOHashMap (TVar (HashMap k v))

-- start helpers
newIOHashMap :: HashMap k v -> STM (IOHashMap k v)
newIOHashMap h = IOHashMap <$> newTVar h

readIOHashMap :: (HashMap k v -> a) -> IOHashMap k v -> STM a
readIOHashMap f (IOHashMap h) = f <$> readTVar h

modifyIOHashMap :: (HashMap k v -> HashMap k v) -> IOHashMap k v -> STM ()
modifyIOHashMap f (IOHashMap h) = modifyTVar' h f
-- end helpers

------------------------------------------------------------------------
-- * Construction

-- | /O(1)/ Construct an empty map.
empty :: STM (IOHashMap k v)
empty = newIOHashMap HM.empty

-- | /O(1)/ Construct a map with a single element.
singleton :: Hashable k => k -> v -> STM (IOHashMap k v)
singleton k = newIOHashMap . HM.singleton k

------------------------------------------------------------------------
-- * Basic interface

-- | /O(1)/ Return 'True' if this map is empty, 'False' otherwise.
null :: IOHashMap k v -> STM Bool
null = readIOHashMap HM.null

-- | /O(n)/ Return the number of key-value mappings in this map.
size :: IOHashMap k v -> STM Int
size = readIOHashMap HM.size

-- | /O(log n)/ Return 'True' if the specified key is present in the
-- map, 'False' otherwise.
member :: (Eq k, Hashable k) => k -> IOHashMap k a -> STM Bool
member = readIOHashMap . HM.member

-- | /O(log n)/ Return the value to which the specified key is mapped,
-- or 'Nothing' if this map contains no mapping for the key.
lookup :: (Eq k, Hashable k) => k -> IOHashMap k v -> STM (Maybe v)
lookup = readIOHashMap . HM.lookup

-- | /O(log n)/ Return the value to which the specified key is mapped,
-- or 'Nothing' if this map contains no mapping for the key.
--
-- This is a flipped version of 'lookup'.
--
-- @since 0.2.11
(!?) :: (Eq k, Hashable k) => IOHashMap k v -> k -> STM (Maybe v)
(!?) m k = lookup k m

-- | /O(log n)/ Return the value to which the specified key is mapped,
-- or the default value if this map contains no mapping for the key.
--
-- @since 0.2.11
findWithDefault :: (Eq k, Hashable k)
              => v          -- ^ Default value to return.
              -> k -> IOHashMap k v -> STM v
findWithDefault v = readIOHashMap . HM.findWithDefault v



-- | /O(log n)/ Return the value to which the specified key is mapped.
-- Calls 'error' if this map contains no mapping for the key.
(!) :: (Eq k, Hashable k) => IOHashMap k v -> k -> STM v
(!) m k = readIOHashMap (HM.! k) m

infixl 9 !

------------------------------------------------------------------------
-- * Basic interface

-- | /O(log n)/ Associate the specified value with the specified
-- key in this map.  If this map previously contained a mapping for
-- the key, the old value is replaced.
insert :: (Eq k, Hashable k) => k -> v -> IOHashMap k v -> STM ()
insert k = modifyIOHashMap . HM.insert k


-- | /O(log n)/ Associate the value with the key in this map.  If
-- this map previously contained a mapping for the key, the old value
-- is replaced by the result of applying the given function to the new
-- and old value.  Example:
--
-- > insertWith f k v map
-- >   where f new old = new + old
insertWith :: (Eq k, Hashable k) => (v -> v -> v) -> k -> v -> IOHashMap k v
           -> STM ()
insertWith f k = modifyIOHashMap . HM.insertWith f k


-- | /O(log n)/ Remove the mapping for the specified key from this map
-- if present.
delete :: (Eq k, Hashable k) => k -> IOHashMap k v -> STM ()
delete = modifyIOHashMap . HM.delete


-- | /O(log n)/ Adjust the value tied to a given key in this map only
-- if it is present. Otherwise, leave the map alone.
adjust :: (Eq k, Hashable k) => (v -> v) -> k -> IOHashMap k v -> STM ()
adjust f = modifyIOHashMap . HM.adjust f

-- | /O(log n)/  The expression @('update' f k map)@ updates the value @x@ at @k@
-- (if it is in the map). If @(f x)@ is 'Nothing', the element is deleted.
-- If it is @('Just' y)@, the key @k@ is bound to the new value @y@.
update :: (Eq k, Hashable k) => (a -> Maybe a) -> k -> IOHashMap k a -> STM ()
update f = modifyIOHashMap . HM.update f

-- | /O(log n)/  The expression @('alter' f k map)@ alters the value @x@ at @k@, or
-- absence thereof.
--
-- 'alter' can be used to insert, delete, or update a value in a map. In short:
--
-- @
-- 'lookup' k ('alter' f k m) = f ('lookup' k m)
-- @
alter :: (Eq k, Hashable k) => (Maybe v -> Maybe v) -> k -> IOHashMap k v -> STM ()
alter f = modifyIOHashMap . HM.alter f

------------------------------------------------------------------------
-- * Folds

-- | /O(n)/ Reduce the map by applying a function to each element
-- and combining the results with a monoid operation.
foldMapWithKey :: Monoid m => (k -> v -> m) -> IOHashMap k v -> STM m
foldMapWithKey = readIOHashMap . HM.foldMapWithKey

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).
foldr :: (v -> a -> a) -> a -> IOHashMap k v -> STM a
foldr f = readIOHashMap . HM.foldr f


-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- left-identity of the operator).
foldl :: (a -> v -> a) -> a -> IOHashMap k v -> STM a
foldl f = readIOHashMap . HM.foldl f


-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).  Each application of the operator
-- is evaluated before using the result in the next application.
-- This function is strict in the starting value.
foldr' :: (v -> a -> a) -> a -> IOHashMap k v -> STM a
foldr' f = readIOHashMap . HM.foldr' f

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- left-identity of the operator).  Each application of the operator
-- is evaluated before using the result in the next application.
-- This function is strict in the starting value.
foldl' :: (a -> v -> a) -> a -> IOHashMap k v -> STM a
foldl' f = readIOHashMap . HM.foldl' f


-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).  Each application of the operator
-- is evaluated before using the result in the next application.
-- This function is strict in the starting value.
foldrWithKey' :: (k -> v -> a -> a) -> a -> IOHashMap k v -> STM a
foldrWithKey' f = readIOHashMap . HM.foldrWithKey' f

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- left-identity of the operator).  Each application of the operator
-- is evaluated before using the result in the next application.
-- This function is strict in the starting value.
foldlWithKey' :: (a -> k -> v -> a) -> a -> IOHashMap k v -> STM a
foldlWithKey' f = readIOHashMap . HM.foldlWithKey' f

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).
foldrWithKey :: (k -> v -> a -> a) -> a -> IOHashMap k v -> STM a
foldrWithKey f = readIOHashMap . HM.foldrWithKey f


-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- left-identity of the operator).
foldlWithKey :: (a -> k -> v -> a) -> a -> IOHashMap k v -> STM a
foldlWithKey f = readIOHashMap . HM.foldlWithKey f


------------------------------------------------------------------------
-- * Conversions

-- TODO: Improve fusion rules by modelled them after the Prelude ones
-- on lists.

-- | /O(n)/ Return a list of this map's keys.  The list is produced
-- lazily.
keys :: IOHashMap k v -> STM [k]
keys = readIOHashMap HM.keys

-- | /O(n)/ Return a list of this map's values.  The list is produced
-- lazily.
elems :: IOHashMap k v -> STM [v]
elems = readIOHashMap HM.elems

------------------------------------------------------------------------
-- ** Lists

-- | /O(n)/ Return a list of this map's elements.  The list is
-- produced lazily. The order of its elements is unspecified.
toList :: IOHashMap k v -> STM [(k, v)]
toList = readIOHashMap HM.toList

-- | /O(n)/ Construct a map with the supplied mappings.  If the list
-- contains duplicate mappings, the later mappings take precedence.
fromList :: (Eq k, Hashable k) => [(k, v)] -> STM (IOHashMap k v)
fromList = newIOHashMap . HM.fromList
