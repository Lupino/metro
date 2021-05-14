module Data.IOHashMap
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

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.STM      (atomically)
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HM
import           Data.Hashable
import           Data.IOHashMap.STM     (IOHashMap)
import qualified Data.IOHashMap.STM     as STM
import           Prelude                hiding (foldl, foldr, lookup, null)

-- start helpers
newIOHashMap :: MonadIO m => HashMap k v -> m (IOHashMap k v)
newIOHashMap = liftIO . atomically . STM.newIOHashMap

readIOHashMap :: MonadIO m => (HashMap k v -> a) -> IOHashMap k v -> m a
readIOHashMap f = liftIO . atomically . STM.readIOHashMap f

modifyIOHashMap :: MonadIO m => (HashMap k v -> HashMap k v) -> IOHashMap k v -> m ()
modifyIOHashMap f = liftIO . atomically . STM.modifyIOHashMap f
-- end helpers

------------------------------------------------------------------------
-- * Construction

-- | /O(1)/ Construct an empty map.
empty :: MonadIO m => m (IOHashMap k v)
empty = newIOHashMap HM.empty

-- | /O(1)/ Construct a map with a single element.
singleton :: MonadIO m => Hashable k => k -> v -> m (IOHashMap k v)
singleton k = newIOHashMap . HM.singleton k

------------------------------------------------------------------------
-- * Basic interface

-- | /O(1)/ Return 'True' if this map is empty, 'False' otherwise.
null :: MonadIO m => IOHashMap k v -> m Bool
null = readIOHashMap HM.null

-- | /O(n)/ Return the number of key-value mappings in this map.
size :: MonadIO m => IOHashMap k v -> m Int
size = readIOHashMap HM.size

-- | /O(log n)/ Return 'True' if the specified key is present in the
-- map, 'False' otherwise.
member :: (MonadIO m, Eq k, Hashable k) => k -> IOHashMap k a -> m Bool
member = readIOHashMap . HM.member

-- | /O(log n)/ Return the value to which the specified key is mapped,
-- or 'Nothing' if this map contains no mapping for the key.
lookup :: (MonadIO m, Eq k, Hashable k) => k -> IOHashMap k v -> m (Maybe v)
lookup = readIOHashMap . HM.lookup

-- | /O(log n)/ Return the value to which the specified key is mapped,
-- or 'Nothing' if this map contains no mapping for the key.
--
-- This is a flipped version of 'lookup'.
--
-- @since 0.2.11
(!?) :: (MonadIO m, Eq k, Hashable k) => IOHashMap k v -> k -> m (Maybe v)
(!?) m k = lookup k m

-- | /O(log n)/ Return the value to which the specified key is mapped,
-- or the default value if this map contains no mapping for the key.
--
-- @since 0.2.11
findWithDefault :: (MonadIO m, Eq k, Hashable k)
              => v          -- ^ Default value to return.
              -> k -> IOHashMap k v -> m v
findWithDefault v = readIOHashMap . HM.findWithDefault v



-- | /O(log n)/ Return the value to which the specified key is mapped.
-- Calls 'error' if this map contains no mapping for the key.
(!) :: (MonadIO m, Eq k, Hashable k) => IOHashMap k v -> k -> m v
(!) m k = readIOHashMap (HM.! k) m

infixl 9 !

------------------------------------------------------------------------
-- * Basic interface

-- | /O(log n)/ Associate the specified value with the specified
-- key in this map.  If this map previously contained a mapping for
-- the key, the old value is replaced.
insert :: (MonadIO m, Eq k, Hashable k) => k -> v -> IOHashMap k v -> m ()
insert k = modifyIOHashMap . HM.insert k


-- | /O(log n)/ Associate the value with the key in this map.  If
-- this map previously contained a mapping for the key, the old value
-- is replaced by the result of applying the given function to the new
-- and old value.  Example:
--
-- > insertWith f k v map
-- >   where f new old = new + old
insertWith :: (MonadIO m, Eq k, Hashable k) => (v -> v -> v) -> k -> v -> IOHashMap k v
           -> m ()
insertWith f k = modifyIOHashMap . HM.insertWith f k


-- | /O(log n)/ Remove the mapping for the specified key from this map
-- if present.
delete :: (MonadIO m, Eq k, Hashable k) => k -> IOHashMap k v -> m ()
delete = modifyIOHashMap . HM.delete


-- | /O(log n)/ Adjust the value tied to a given key in this map only
-- if it is present. Otherwise, leave the map alone.
adjust :: MonadIO m => (Eq k, Hashable k) => (v -> v) -> k -> IOHashMap k v -> m ()
adjust f = modifyIOHashMap . HM.adjust f

-- | /O(log n)/  The expression @('update' f k map)@ updates the value @x@ at @k@
-- (if it is in the map). If @(f x)@ is 'Nothing', the element is deleted.
-- If it is @('Just' y)@, the key @k@ is bound to the new value @y@.
update :: (MonadIO m, Eq k, Hashable k) => (a -> Maybe a) -> k -> IOHashMap k a -> m ()
update f = modifyIOHashMap . HM.update f

-- | /O(log n)/  The expression @('alter' f k map)@ alters the value @x@ at @k@, or
-- absence thereof.
--
-- 'alter' can be used to insert, delete, or update a value in a map. In short:
--
-- @
-- 'lookup' k ('alter' f k m) = f ('lookup' k m)
-- @
alter :: (MonadIO m, Eq k, Hashable k) => (Maybe v -> Maybe v) -> k -> IOHashMap k v -> m ()
alter f = modifyIOHashMap . HM.alter f

------------------------------------------------------------------------
-- * Folds

-- | /O(n)/ Reduce the map by applying a function to each element
-- and combining the results with a monoid operation.
foldMapWithKey ::(MonadIO m, Monoid n) => (k -> v -> n) -> IOHashMap k v -> m n
foldMapWithKey = readIOHashMap . HM.foldMapWithKey

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).
foldr :: MonadIO m => (v -> a -> a) -> a -> IOHashMap k v -> m a
foldr f = readIOHashMap . HM.foldr f


-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- left-identity of the operator).
foldl :: MonadIO m => (a -> v -> a) -> a -> IOHashMap k v -> m a
foldl f = readIOHashMap . HM.foldl f


-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).  Each application of the operator
-- is evaluated before using the result in the next application.
-- This function is strict in the starting value.
foldr' :: MonadIO m => (v -> a -> a) -> a -> IOHashMap k v -> m a
foldr' f = readIOHashMap . HM.foldr' f

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- left-identity of the operator).  Each application of the operator
-- is evaluated before using the result in the next application.
-- This function is strict in the starting value.
foldl' :: MonadIO m => (a -> v -> a) -> a -> IOHashMap k v -> m a
foldl' f = readIOHashMap . HM.foldl' f


-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).  Each application of the operator
-- is evaluated before using the result in the next application.
-- This function is strict in the starting value.
foldrWithKey' :: MonadIO m => (k -> v -> a -> a) -> a -> IOHashMap k v -> m a
foldrWithKey' f = readIOHashMap . HM.foldrWithKey' f

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- left-identity of the operator).  Each application of the operator
-- is evaluated before using the result in the next application.
-- This function is strict in the starting value.
foldlWithKey' :: MonadIO m => (a -> k -> v -> a) -> a -> IOHashMap k v -> m a
foldlWithKey' f = readIOHashMap . HM.foldlWithKey' f

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).
foldrWithKey :: MonadIO m => (k -> v -> a -> a) -> a -> IOHashMap k v -> m a
foldrWithKey f = readIOHashMap . HM.foldrWithKey f


-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- left-identity of the operator).
foldlWithKey :: MonadIO m => (a -> k -> v -> a) -> a -> IOHashMap k v -> m a
foldlWithKey f = readIOHashMap . HM.foldlWithKey f


------------------------------------------------------------------------
-- * Conversions

-- TODO: Improve fusion rules by modelled them after the Prelude ones
-- on lists.

-- | /O(n)/ Return a list of this map's keys.  The list is produced
-- lazily.
keys :: MonadIO m => IOHashMap k v -> m [k]
keys = readIOHashMap HM.keys

-- | /O(n)/ Return a list of this map's values.  The list is produced
-- lazily.
elems :: MonadIO m => IOHashMap k v -> m [v]
elems = readIOHashMap HM.elems

------------------------------------------------------------------------
-- ** Lists

-- | /O(n)/ Return a list of this map's elements.  The list is
-- produced lazily. The order of its elements is unspecified.
toList :: MonadIO m => IOHashMap k v -> m [(k, v)]
toList = readIOHashMap HM.toList

-- | /O(n)/ Construct a map with the supplied mappings.  If the list
-- contains duplicate mappings, the later mappings take precedence.
fromList :: (MonadIO m, Eq k, Hashable k) => [(k, v)] -> m (IOHashMap k v)
fromList = newIOHashMap . HM.fromList
