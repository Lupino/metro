module Data.IOMap
  ( IOMap
  , newIOMap
  , readIOMap
  , modifyIOMap
  , (!)
  , (!?)
  , null
  , size
  , lookup
  , member
  , notMember
  , findWithDefault
  , lookupLT
  , lookupGT
  , lookupLE
  , lookupGE
  , empty
  , singleton
  , insert
  , insertWith
  , insertWithKey
  , insertLookupWithKey
  , delete
  , adjust
  , adjustWithKey
  , update
  , updateWithKey
  , updateLookupWithKey
  , alter
  , findIndex
  , lookupIndex
  , elemAt
  , updateAt
  , deleteAt
  , lookupMin
  , findMin
  , lookupMax
  , findMax
  , deleteMin
  , deleteMax
  , updateMin
  , updateMax
  , updateMinWithKey
  , updateMaxWithKey
  , foldr
  , foldr'
  , foldl
  , foldl'
  , foldrWithKey
  , foldrWithKey'
  , foldlWithKey
  , foldlWithKey'
  , foldMapWithKey
  , elems
  , keys
  , assocs
  , keysSet
  , fromSet
  , fromList
  , fromListWith
  , fromListWithKey
  , toList
  , toAscList
  , toDescList
  , fromAscList
  , fromDescList
  , fromAscListWith
  , fromDescListWith
  , fromAscListWithKey
  , fromDescListWithKey
  , fromDistinctAscList
  , fromDistinctDescList
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.STM      (atomically)
import           Data.IOMap.STM         (IOMap)
import qualified Data.IOMap.STM         as STM
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import qualified Data.Set.Internal      as Set
import           Prelude                hiding (foldl, foldr, lookup, null)

-- start helpers
newIOMap :: MonadIO m => Map k v -> m (IOMap k v)
newIOMap = liftIO . atomically . STM.newIOMap

readIOMap :: MonadIO m => (Map k v -> a) -> IOMap k v -> m a
readIOMap f = liftIO . atomically . STM.readIOMap f

modifyIOMap :: MonadIO m => (Map k v -> Map k v) -> IOMap k v -> m ()
modifyIOMap f = liftIO . atomically . STM.modifyIOMap f
-- end helpers

{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}
infixl 9 !,!? --

-- | /O(log n)/. Find the value at a key.
-- Calls 'error' when the element can not be found.
--
-- > fromList [(5,'a'), (3,'b')] ! 1    Error: element not in the map
-- > fromList [(5,'a'), (3,'b')] ! 5 == 'a'

(!) :: (MonadIO m, Ord k) => IOMap k a -> k -> m a
(!) m k = readIOMap (Map.! k) m

-- | /O(log n)/. Find the value at a key.
-- Returns 'Nothing' when the element can not be found.
--
-- prop> fromList [(5, 'a'), (3, 'b')] !? 1 == Nothing
-- prop> fromList [(5, 'a'), (3, 'b')] !? 5 == Just 'a'
--
-- @since 0.5.9

(!?) :: (MonadIO m, Ord k) => IOMap k a -> k -> m (Maybe a)
(!?) m k = lookup k m

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}
-- | /O(1)/. Is the map empty?
--
-- > Data.Map.null (empty)           == True
-- > Data.Map.null (singleton 1 'a') == False

null :: MonadIO m => IOMap k a -> m Bool
null = readIOMap Map.null

-- | /O(1)/. The number of elements in the map.
--
-- > size empty                                   == 0
-- > size (singleton 1 'a')                       == 1
-- > size (fromList([(1,'a'), (2,'c'), (3,'b')])) == 3

size :: MonadIO m => IOMap k a -> m Int
size = readIOMap Map.size

-- | /O(log n)/. Lookup the value at a key in the map.
--
-- The function will return the corresponding value as @('Just' value)@,
-- or 'Nothing' if the key isn't in the map.
--
-- An example of using @lookup@:
--
-- > import Prelude hiding (lookup)
-- > import Data.Map
-- >
-- > employeeDept = fromList([("John","Sales"), ("Bob","IT")])
-- > deptCountry = fromList([("IT","USA"), ("Sales","France")])
-- > countryCurrency = fromList([("USA", "Dollar"), ("France", "Euro")])
-- >
-- > employeeCurrency :: MonadIO m => String -> Maybe String
-- > employeeCurrency name = do
-- >     dept <- lookup name employeeDept
-- >     country <- lookup dept deptCountry
-- >     lookup country countryCurrency
-- >
-- > main = do
-- >     putStrLn $ "John's currency: " ++ (show (employeeCurrency "John"))
-- >     putStrLn $ "Pete's currency: " ++ (show (employeeCurrency "Pete"))
--
-- The output of this program:
--
-- >   John's currency: Just "Euro"
-- >   Pete's currency: Nothing
lookup :: (MonadIO m, Ord k) => k -> IOMap k a -> m (Maybe a)
lookup = readIOMap . Map.lookup

-- | /O(log n)/. Is the key a member of the map? See also 'notMember'.
--
-- > member 5 (fromList [(5,'a'), (3,'b')]) == True
-- > member 1 (fromList [(5,'a'), (3,'b')]) == False
member :: (MonadIO m, Ord k) => k -> IOMap k a -> m Bool
member = readIOMap . Map.member

-- | /O(log n)/. Is the key not a member of the map? See also 'member'.
--
-- > notMember 5 (fromList [(5,'a'), (3,'b')]) == False
-- > notMember 1 (fromList [(5,'a'), (3,'b')]) == True

notMember :: (MonadIO m, Ord k) => k -> IOMap k a -> m Bool
notMember = readIOMap . Map.notMember

-- | /O(log n)/. The expression @('findWithDefault' def k map)@ returns
-- the value at key @k@ or returns default value @def@
-- when the key is not in the map.
--
-- > findWithDefault 'x' 1 (fromList [(5,'a'), (3,'b')]) == 'x'
-- > findWithDefault 'x' 5 (fromList [(5,'a'), (3,'b')]) == 'a'
findWithDefault :: (MonadIO m, Ord k) => a -> k -> IOMap k a -> m a
findWithDefault a = readIOMap . Map.findWithDefault a

-- | /O(log n)/. Find largest key smaller than the given one and return the
-- corresponding (key, value) pair.
--
-- > lookupLT 3 (fromList [(3,'a'), (5,'b')]) == Nothing
-- > lookupLT 4 (fromList [(3,'a'), (5,'b')]) == Just (3, 'a')
lookupLT :: (MonadIO m, Ord k) => k -> IOMap k v -> m (Maybe (k, v))
lookupLT = readIOMap . Map.lookupLT

-- | /O(log n)/. Find smallest key greater than the given one and return the
-- corresponding (key, value) pair.
--
-- > lookupGT 4 (fromList [(3,'a'), (5,'b')]) == Just (5, 'b')
-- > lookupGT 5 (fromList [(3,'a'), (5,'b')]) == Nothing
lookupGT :: (MonadIO m, Ord k) => k -> IOMap k v -> m (Maybe (k, v))
lookupGT = readIOMap . Map.lookupGT

-- | /O(log n)/. Find largest key smaller or equal to the given one and return
-- the corresponding (key, value) pair.
--
-- > lookupLE 2 (fromList [(3,'a'), (5,'b')]) == Nothing
-- > lookupLE 4 (fromList [(3,'a'), (5,'b')]) == Just (3, 'a')
-- > lookupLE 5 (fromList [(3,'a'), (5,'b')]) == Just (5, 'b')
lookupLE :: (MonadIO m, Ord k) => k -> IOMap k v -> m (Maybe (k, v))
lookupLE = readIOMap . Map.lookupLE

-- | /O(log n)/. Find smallest key greater or equal to the given one and return
-- the corresponding (key, value) pair.
--
-- > lookupGE 3 (fromList [(3,'a'), (5,'b')]) == Just (3, 'a')
-- > lookupGE 4 (fromList [(3,'a'), (5,'b')]) == Just (5, 'b')
-- > lookupGE 6 (fromList [(3,'a'), (5,'b')]) == Nothing
lookupGE :: (MonadIO m, Ord k) => k -> IOMap k v -> m (Maybe (k, v))
lookupGE = readIOMap . Map.lookupGE

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
-- | /O(1)/. The empty map.
--
-- > empty      == fromList []
-- > size empty == 0

empty :: MonadIO m => m (IOMap k a)
empty = newIOMap Map.empty

-- | /O(1)/. A map with a single element.
--
-- > singleton 1 'a'        == fromList [(1, 'a')]
-- > size (singleton 1 'a') == 1

singleton :: MonadIO m => k -> a -> m (IOMap k a)
singleton k = newIOMap . Map.singleton k

{--------------------------------------------------------------------
  Insertion
--------------------------------------------------------------------}
-- | /O(log n)/. Insert a new key and value in the map.
-- If the key is already present in the map, the associated value is
-- replaced with the supplied value. 'insert' is equivalent to
-- @'insertWith' 'const'@.
--
-- > insert 5 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3, 'b'), (5, 'x')]
-- > insert 7 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3, 'b'), (5, 'a'), (7, 'x')]
-- > insert 5 'x' empty                         == singleton 5 'x'

-- See Note: Type of local 'go' function
-- See Note: Avoiding worker/wrapper
insert :: (MonadIO m, Ord k) => k -> a -> IOMap k a -> m ()
insert k = modifyIOMap . Map.insert k

-- | /O(log n)/. Insert with a function, combining new value and old value.
-- @'insertWith' f key value mp@
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert the pair @(key, f new_value old_value)@.
--
-- > insertWith (++) 5 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "xxxa")]
-- > insertWith (++) 7 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "xxx")]
-- > insertWith (++) 5 "xxx" empty                         == singleton 5 "xxx"

insertWith :: (MonadIO m, Ord k) => (a -> a -> a) -> k -> a -> IOMap k a -> m ()
insertWith f k = modifyIOMap . Map.insertWith f k

-- | /O(log n)/. Insert with a function, combining key, new value and old value.
-- @'insertWithKey' f key value mp@
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert the pair @(key,f key new_value old_value)@.
-- Note that the key passed to f is the same key passed to 'insertWithKey'.
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > insertWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:xxx|a")]
-- > insertWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "xxx")]
-- > insertWithKey f 5 "xxx" empty                         == singleton 5 "xxx"

-- See Note: Type of local 'go' function
insertWithKey :: (MonadIO m, Ord k) => (k -> a -> a -> a) -> k -> a -> IOMap k a -> m ()
insertWithKey f k = modifyIOMap . Map.insertWithKey f k

-- | /O(log n)/. Combines insert operation with old value retrieval.
-- The expression (@'insertLookupWithKey' f k x map@)
-- is a pair where the first element is equal to (@'lookup' k map@)
-- and the second element equal to (@'insertWithKey' f k x map@).
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > insertLookupWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) == (Just "a", fromList [(3, "b"), (5, "5:xxx|a")])
-- > insertLookupWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a"), (7, "xxx")])
-- > insertLookupWithKey f 5 "xxx" empty                         == (Nothing,  singleton 5 "xxx")
--
-- This is how to define @insertLookup@ using @insertLookupWithKey@:
--
-- > let insertLookup kx x t = insertLookupWithKey (\_ a _ -> a) kx x t
-- > insertLookup 5 "x" (fromList [(5,"a"), (3,"b")]) == (Just "a", fromList [(3, "b"), (5, "x")])
-- > insertLookup 7 "x" (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a"), (7, "x")])

-- See Note: Type of local 'go' function
insertLookupWithKey :: (MonadIO m, Ord k) => (k -> a -> a -> a) -> k -> a -> IOMap k a
                    -> m (Maybe a)
insertLookupWithKey f k a m = fst <$> readIOMap (Map.insertLookupWithKey f k a) m

{--------------------------------------------------------------------
  Deletion
--------------------------------------------------------------------}
-- | /O(log n)/. Delete a key and its value from the map. When the key is not
-- a member of the map, the original map is returned.
--
-- > delete 5 (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > delete 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > delete 5 empty                         == empty

-- See Note: Type of local 'go' function
delete :: (MonadIO m, Ord k) => k -> IOMap k a -> m ()
delete = modifyIOMap . Map.delete

-- | /O(log n)/. Update a value at a specific key with the result of the provided function.
-- When the key is not
-- a member of the map, the original map is returned.
--
-- > adjust ("new " ++) 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
-- > adjust ("new " ++) 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > adjust ("new " ++) 7 empty                         == empty

adjust :: (MonadIO m, Ord k) => (a -> a) -> k -> IOMap k a -> m ()
adjust f = modifyIOMap . Map.adjust f

-- | /O(log n)/. Adjust a value at a specific key. When the key is not
-- a member of the map, the original map is returned.
--
-- > let f key x = (show key) ++ ":new " ++ x
-- > adjustWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
-- > adjustWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > adjustWithKey f 7 empty                         == empty

adjustWithKey :: (MonadIO m, Ord k) => (k -> a -> a) -> k -> IOMap k a -> m ()
adjustWithKey f = modifyIOMap . Map.adjustWithKey f

-- | /O(log n)/. The expression (@'update' f k map@) updates the value @x@
-- at @k@ (if it is in the map). If (@f x@) is 'Nothing', the element is
-- deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
--
-- > let f x = if x == "a" then Just "new a" else Nothing
-- > update f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
-- > update f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > update f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

update :: (MonadIO m, Ord k) => (a -> Maybe a) -> k -> IOMap k a -> m ()
update f = modifyIOMap . Map.update f

-- | /O(log n)/. The expression (@'updateWithKey' f k map@) updates the
-- value @x@ at @k@ (if it is in the map). If (@f k x@) is 'Nothing',
-- the element is deleted. If it is (@'Just' y@), the key @k@ is bound
-- to the new value @y@.
--
-- > let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
-- > updateWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
-- > updateWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > updateWithKey f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

-- See Note: Type of local 'go' function
updateWithKey :: (MonadIO m, Ord k) => (k -> a -> Maybe a) -> k -> IOMap k a -> m ()
updateWithKey f = modifyIOMap . Map.updateWithKey f

-- | /O(log n)/. Lookup and update. See also 'updateWithKey'.
-- The function returns changed value, if it is updated.
-- Returns the original key value if the map entry is deleted.
--
-- > let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
-- > updateLookupWithKey f 5 (fromList [(5,"a"), (3,"b")]) == (Just "5:new a", fromList [(3, "b"), (5, "5:new a")])
-- > updateLookupWithKey f 7 (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a")])
-- > updateLookupWithKey f 3 (fromList [(5,"a"), (3,"b")]) == (Just "b", singleton 5 "a")

-- See Note: Type of local 'go' function
updateLookupWithKey :: (MonadIO m, Ord k) => (k -> a -> Maybe a) -> k -> IOMap k a -> m (Maybe a)
updateLookupWithKey f k m = fst <$> readIOMap (Map.updateLookupWithKey f k) m

-- | /O(log n)/. The expression (@'alter' f k map@) alters the value @x@ at @k@, or absence thereof.
-- 'alter' can be used to insert, delete, or update a value in a 'Map'.
-- In short : @'lookup' k ('alter' f k m) = f ('lookup' k m)@.
--
-- > let f _ = Nothing
-- > alter f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > alter f 5 (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- >
-- > let f _ = Just "c"
-- > alter f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "c")]
-- > alter f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "c")]

-- See Note: Type of local 'go' function
alter :: (MonadIO m, Ord k) => (Maybe a -> Maybe a) -> k -> IOMap k a -> m ()
alter f = modifyIOMap . Map.alter f

{--------------------------------------------------------------------
  Indexing
--------------------------------------------------------------------}
-- | /O(log n)/. Return the /index/ of a key, which is its zero-based index in
-- the sequence sorted by keys. The index is a number from /0/ up to, but not
-- including, the 'size' of the map. Calls 'error' when the key is not
-- a 'member' of the map.
--
-- > findIndex 2 (fromList [(5,"a"), (3,"b")])    Error: element is not in the map
-- > findIndex 3 (fromList [(5,"a"), (3,"b")]) == 0
-- > findIndex 5 (fromList [(5,"a"), (3,"b")]) == 1
-- > findIndex 6 (fromList [(5,"a"), (3,"b")])    Error: element is not in the map

-- See Note: Type of local 'go' function
findIndex :: (MonadIO m, Ord k) => k -> IOMap k a -> m Int
findIndex = readIOMap . Map.findIndex

-- | /O(log n)/. Lookup the /index/ of a key, which is its zero-based index in
-- the sequence sorted by keys. The index is a number from /0/ up to, but not
-- including, the 'size' of the map.
--
-- > isJust (lookupIndex 2 (fromList [(5,"a"), (3,"b")]))   == False
-- > fromJust (lookupIndex 3 (fromList [(5,"a"), (3,"b")])) == 0
-- > fromJust (lookupIndex 5 (fromList [(5,"a"), (3,"b")])) == 1
-- > isJust (lookupIndex 6 (fromList [(5,"a"), (3,"b")]))   == False

-- See Note: Type of local 'go' function
lookupIndex :: (MonadIO m, Ord k) => k -> IOMap k a -> m (Maybe Int)
lookupIndex = readIOMap . Map.lookupIndex

-- | /O(log n)/. Retrieve an element by its /index/, i.e. by its zero-based
-- index in the sequence sorted by keys. If the /index/ is out of range (less
-- than zero, greater or equal to 'size' of the map), 'error' is called.
--
-- > elemAt 0 (fromList [(5,"a"), (3,"b")]) == (3,"b")
-- > elemAt 1 (fromList [(5,"a"), (3,"b")]) == (5, "a")
-- > elemAt 2 (fromList [(5,"a"), (3,"b")])    Error: index out of range

elemAt :: MonadIO m => Int -> IOMap k a -> m (k,a)
elemAt = readIOMap . Map.elemAt

-- | /O(log n)/. Update the element at /index/, i.e. by its zero-based index in
-- the sequence sorted by keys. If the /index/ is out of range (less than zero,
-- greater or equal to 'size' of the map), 'error' is called.
--
-- > updateAt (\ _ _ -> Just "x") 0    (fromList [(5,"a"), (3,"b")]) == fromList [(3, "x"), (5, "a")]
-- > updateAt (\ _ _ -> Just "x") 1    (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "x")]
-- > updateAt (\ _ _ -> Just "x") 2    (fromList [(5,"a"), (3,"b")])    Error: index out of range
-- > updateAt (\ _ _ -> Just "x") (-1) (fromList [(5,"a"), (3,"b")])    Error: index out of range
-- > updateAt (\_ _  -> Nothing)  0    (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
-- > updateAt (\_ _  -> Nothing)  1    (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > updateAt (\_ _  -> Nothing)  2    (fromList [(5,"a"), (3,"b")])    Error: index out of range
-- > updateAt (\_ _  -> Nothing)  (-1) (fromList [(5,"a"), (3,"b")])    Error: index out of range

updateAt :: MonadIO m => (k -> a -> Maybe a) -> Int -> IOMap k a -> m ()
updateAt f = modifyIOMap . Map.updateAt f

-- | /O(log n)/. Delete the element at /index/, i.e. by its zero-based index in
-- the sequence sorted by keys. If the /index/ is out of range (less than zero,
-- greater or equal to 'size' of the map), 'error' is called.
--
-- > deleteAt 0  (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
-- > deleteAt 1  (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > deleteAt 2 (fromList [(5,"a"), (3,"b")])     Error: index out of range
-- > deleteAt (-1) (fromList [(5,"a"), (3,"b")])  Error: index out of range

deleteAt :: MonadIO m => Int -> IOMap k a -> m ()
deleteAt = modifyIOMap . Map.deleteAt


{--------------------------------------------------------------------
  Minimal, Maximal
--------------------------------------------------------------------}

-- | /O(log n)/. The minimal key of the map. Returns 'Nothing' if the map is empty.
--
-- > lookupMin (fromList [(5,"a"), (3,"b")]) == Just (3,"b")
-- > lookupMin empty = Nothing
--
-- @since 0.5.9

lookupMin :: MonadIO m => IOMap k a -> m (Maybe (k,a))
lookupMin = readIOMap Map.lookupMin

-- | /O(log n)/. The minimal key of the map. Calls 'error' if the map is empty.
--
-- > findMin (fromList [(5,"a"), (3,"b")]) == (3,"b")
-- > findMin empty                            Error: empty map has no minimal element

findMin :: MonadIO m => IOMap k a -> m (k,a)
findMin = readIOMap Map.findMin

-- | /O(log n)/. The maximal key of the map. Returns 'Nothing' if the map is empty.
--
-- > lookupMax (fromList [(5,"a"), (3,"b")]) == Just (5,"a")
-- > lookupMax empty = Nothing
--
-- @since 0.5.9

lookupMax :: MonadIO m => IOMap k a -> m (Maybe (k, a))
lookupMax = readIOMap Map.lookupMax

-- | /O(log n)/. The maximal key of the map. Calls 'error' if the map is empty.
--
-- > findMax (fromList [(5,"a"), (3,"b")]) == (5,"a")
-- > findMax empty                            Error: empty map has no maximal element

findMax :: MonadIO m => IOMap k a -> m (k,a)
findMax = readIOMap Map.findMax

-- | /O(log n)/. Delete the minimal key. Returns an empty map if the map is empty.
--
-- > deleteMin (fromList [(5,"a"), (3,"b"), (7,"c")]) == fromList [(5,"a"), (7,"c")]
-- > deleteMin empty == empty

deleteMin :: MonadIO m => IOMap k a -> m ()
deleteMin = modifyIOMap Map.deleteMin

-- | /O(log n)/. Delete the maximal key. Returns an empty map if the map is empty.
--
-- > deleteMax (fromList [(5,"a"), (3,"b"), (7,"c")]) == fromList [(3,"b"), (5,"a")]
-- > deleteMax empty == empty

deleteMax :: MonadIO m => IOMap k a -> m ()
deleteMax = modifyIOMap Map.deleteMax

-- | /O(log n)/. Update the value at the minimal key.
--
-- > updateMin (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3, "Xb"), (5, "a")]
-- > updateMin (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

updateMin :: MonadIO m => (a -> Maybe a) -> IOMap k a -> m ()
updateMin = modifyIOMap . Map.updateMin

-- | /O(log n)/. Update the value at the maximal key.
--
-- > updateMax (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "Xa")]
-- > updateMax (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"

updateMax :: MonadIO m => (a -> Maybe a) -> IOMap k a -> m ()
updateMax = modifyIOMap . Map.updateMax

-- | /O(log n)/. Update the value at the minimal key.
--
-- > updateMinWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3,"3:b"), (5,"a")]
-- > updateMinWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

updateMinWithKey :: MonadIO m => (k -> a -> Maybe a) -> IOMap k a -> m ()
updateMinWithKey = modifyIOMap . Map.updateMinWithKey

-- | /O(log n)/. Update the value at the maximal key.
--
-- > updateMaxWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3,"b"), (5,"5:a")]
-- > updateMaxWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"

updateMaxWithKey :: MonadIO m => (k -> a -> Maybe a) -> IOMap k a -> m ()
updateMaxWithKey = modifyIOMap . Map.updateMaxWithKey

{--------------------------------------------------------------------
  Folds
--------------------------------------------------------------------}

-- | /O(n)/. Fold the values in the map using the given right-associative
-- binary operator, such that @'foldr' f z == 'Prelude.foldr' f z . 'elems'@.
--
-- For example,
--
-- > elems map = foldr (:) [] map
--
-- > let f a len = len + (length a)
-- > foldr f 0 (fromList [(5,"a"), (3,"bbb")]) == 4
foldr :: MonadIO m => (a -> b -> b) -> b -> IOMap k a -> m b
foldr f = readIOMap . Map.foldr f

-- | /O(n)/. A strict version of 'foldr'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldr' :: MonadIO m => (a -> b -> b) -> b -> IOMap k a -> m b
foldr' f = readIOMap . Map.foldr' f

-- | /O(n)/. Fold the values in the map using the given left-associative
-- binary operator, such that @'foldl' f z == 'Prelude.foldl' f z . 'elems'@.
--
-- For example,
--
-- > elems = reverse . foldl (flip (:)) []
--
-- > let f len a = len + (length a)
-- > foldl f 0 (fromList [(5,"a"), (3,"bbb")]) == 4
foldl :: MonadIO m => (a -> b -> a) -> a -> IOMap k b -> m a
foldl f = readIOMap . Map.foldl f

-- | /O(n)/. A strict version of 'foldl'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldl' :: MonadIO m => (a -> b -> a) -> a -> IOMap k b -> m a
foldl' f = readIOMap . Map.foldl' f

-- | /O(n)/. Fold the keys and values in the map using the given right-associative
-- binary operator, such that
-- @'foldrWithKey' f z == 'Prelude.foldr' ('uncurry' f) z . 'toAscList'@.
--
-- For example,
--
-- > keys map = foldrWithKey (\k x ks -> k:ks) [] map
--
-- > let f k a result = result ++ "(" ++ (show k) ++ ":" ++ a ++ ")"
-- > foldrWithKey f "Map: " (fromList [(5,"a"), (3,"b")]) == "Map: (5:a)(3:b)"
foldrWithKey :: MonadIO m => (k -> a -> b -> b) -> b -> IOMap k a -> m b
foldrWithKey f = readIOMap . Map.foldrWithKey f

-- | /O(n)/. A strict version of 'foldrWithKey'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldrWithKey' :: MonadIO m => (k -> a -> b -> b) -> b -> IOMap k a -> m b
foldrWithKey' f = readIOMap . Map.foldrWithKey' f

-- | /O(n)/. Fold the keys and values in the map using the given left-associative
-- binary operator, such that
-- @'foldlWithKey' f z == 'Prelude.foldl' (\\z' (kx, x) -> f z' kx x) z . 'toAscList'@.
--
-- For example,
--
-- > keys = reverse . foldlWithKey (\ks k x -> k:ks) []
--
-- > let f result k a = result ++ "(" ++ (show k) ++ ":" ++ a ++ ")"
-- > foldlWithKey f "Map: " (fromList [(5,"a"), (3,"b")]) == "Map: (3:b)(5:a)"
foldlWithKey :: MonadIO m => (a -> k -> b -> a) -> a -> IOMap k b -> m a
foldlWithKey f = readIOMap . Map.foldlWithKey f

-- | /O(n)/. A strict version of 'foldlWithKey'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldlWithKey' :: MonadIO m => (a -> k -> b -> a) -> a -> IOMap k b -> m a
foldlWithKey' f = readIOMap . Map.foldlWithKey' f

-- | /O(n)/. Fold the keys and values in the map using the given monoid, such that
--
-- @'foldMapWithKey' f = 'Prelude.fold' . 'mapWithKey' f@
--
-- This can be an asymptotically faster than 'foldrWithKey' or 'foldlWithKey' for some monoids.
--
-- @since 0.5.4
foldMapWithKey :: MonadIO m => Monoid n => (k -> a -> n) -> IOMap k a -> m n
foldMapWithKey = readIOMap . Map.foldMapWithKey

{--------------------------------------------------------------------
  List variations
--------------------------------------------------------------------}
-- | /O(n)/.
-- Return all elements of the map in the ascending order of their keys.
-- Subject to list fusion.
--
-- > elems (fromList [(5,"a"), (3,"b")]) == ["b","a"]
-- > elems empty == []

elems :: MonadIO m => IOMap k a -> m [a]
elems = readIOMap Map.elems

-- | /O(n)/. Return all keys of the map in ascending order. Subject to list
-- fusion.
--
-- > keys (fromList [(5,"a"), (3,"b")]) == [3,5]
-- > keys empty == []

keys  :: MonadIO m => IOMap k a -> m [k]
keys = readIOMap Map.keys

-- | /O(n)/. An alias for 'toAscList'. Return all key\/value pairs in the map
-- in ascending key order. Subject to list fusion.
--
-- > assocs (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
-- > assocs empty == []

assocs :: MonadIO m => IOMap k a -> m [(k,a)]
assocs = readIOMap Map.assocs

-- | /O(n)/. The set of all keys of the map.
--
-- > keysSet (fromList [(5,"a"), (3,"b")]) == Data.Set.fromList [3,5]
-- > keysSet empty == Data.Set.empty

keysSet :: MonadIO m => IOMap k a -> m (Set.Set k)
keysSet = readIOMap Map.keysSet

-- | /O(n)/. Build a map from a set of keys and a function which for each key
-- computes its value.
--
-- > fromSet (\k -> replicate k 'a') (Data.Set.fromList [3, 5]) == fromList [(5,"aaaaa"), (3,"aaa")]
-- > fromSet undefined Data.Set.empty == empty

fromSet :: MonadIO m => (k -> a) -> Set.Set k -> m (IOMap k a)
fromSet f = newIOMap . Map.fromSet f

{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}

-- | /O(n*log n)/. Build a map from a list of key\/value pairs. See also 'fromAscList'.
-- If the list contains more than one value for the same key, the last value
-- for the key is retained.
--
-- If the keys of the list are ordered, linear-time implementation is used,
-- with the performance equal to 'fromDistinctAscList'.
--
-- > fromList [] == empty
-- > fromList [(5,"a"), (3,"b"), (5, "c")] == fromList [(5,"c"), (3,"b")]
-- > fromList [(5,"c"), (3,"b"), (5, "a")] == fromList [(5,"a"), (3,"b")]

-- For some reason, when 'singleton' is used in fromList or in
-- create, it is not inlined, so we inline it manually.
fromList :: (MonadIO m, Ord k) => [(k,a)] -> m (IOMap k a)
fromList = newIOMap . Map.fromList

-- | /O(n*log n)/. Build a map from a list of key\/value pairs with a combining function. See also 'fromAscListWith'.
--
-- > fromListWith (++) [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"a")] == fromList [(3, "ab"), (5, "aba")]
-- > fromListWith (++) [] == empty

fromListWith :: (MonadIO m, Ord k) => (a -> a -> a) -> [(k,a)] -> m (IOMap k a)
fromListWith f = newIOMap . Map.fromListWith f

-- | /O(n*log n)/. Build a map from a list of key\/value pairs with a combining function. See also 'fromAscListWithKey'.
--
-- > let f k a1 a2 = (show k) ++ a1 ++ a2
-- > fromListWithKey f [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"a")] == fromList [(3, "3ab"), (5, "5a5ba")]
-- > fromListWithKey f [] == empty

fromListWithKey :: (MonadIO m, Ord k) => (k -> a -> a -> a) -> [(k,a)] -> m (IOMap k a)
fromListWithKey f = newIOMap . Map.fromListWithKey f

-- | /O(n)/. Convert the map to a list of key\/value pairs. Subject to list fusion.
--
-- > toList (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
-- > toList empty == []

toList :: MonadIO m => IOMap k a -> m [(k,a)]
toList = readIOMap Map.toList

-- | /O(n)/. Convert the map to a list of key\/value pairs where the keys are
-- in ascending order. Subject to list fusion.
--
-- > toAscList (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]

toAscList :: MonadIO m => IOMap k a -> m [(k,a)]
toAscList = readIOMap Map.toAscList

-- | /O(n)/. Convert the map to a list of key\/value pairs where the keys
-- are in descending order. Subject to list fusion.
--
-- > toDescList (fromList [(5,"a"), (3,"b")]) == [(5,"a"), (3,"b")]

toDescList :: MonadIO m => IOMap k a -> m [(k,a)]
toDescList = readIOMap Map.toDescList

{--------------------------------------------------------------------
  Building trees from ascending/descending lists can be done in linear time.

  Note that if [xs] is ascending that:
    fromAscList xs       == fromList xs
    fromAscListWith f xs == fromListWith f xs
--------------------------------------------------------------------}
-- | /O(n)/. Build a map from an ascending list in linear time.
-- /The precondition (input list is ascending) is not checked./
--
-- > fromAscList [(3,"b"), (5,"a")]          == fromList [(3, "b"), (5, "a")]
-- > fromAscList [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "b")]
-- > valid (fromAscList [(3,"b"), (5,"a"), (5,"b")]) == True
-- > valid (fromAscList [(5,"a"), (3,"b"), (5,"b")]) == False

fromAscList :: (MonadIO m, Eq k) => [(k,a)] -> m (IOMap k a)
fromAscList = newIOMap . Map.fromAscList

-- | /O(n)/. Build a map from a descending list in linear time.
-- /The precondition (input list is descending) is not checked./
--
-- > fromDescList [(5,"a"), (3,"b")]          == fromList [(3, "b"), (5, "a")]
-- > fromDescList [(5,"a"), (5,"b"), (3,"b")] == fromList [(3, "b"), (5, "b")]
-- > valid (fromDescList [(5,"a"), (5,"b"), (3,"b")]) == True
-- > valid (fromDescList [(5,"a"), (3,"b"), (5,"b")]) == False
--
-- @since 0.5.8

fromDescList :: (MonadIO m, Eq k) => [(k,a)] -> m (IOMap k a)
fromDescList = newIOMap . Map.fromDescList

-- | /O(n)/. Build a map from an ascending list in linear time with a combining function for equal keys.
-- /The precondition (input list is ascending) is not checked./
--
-- > fromAscListWith (++) [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "ba")]
-- > valid (fromAscListWith (++) [(3,"b"), (5,"a"), (5,"b")]) == True
-- > valid (fromAscListWith (++) [(5,"a"), (3,"b"), (5,"b")]) == False

fromAscListWith :: (MonadIO m, Eq k) => (a -> a -> a) -> [(k,a)] -> m (IOMap k a)
fromAscListWith f = newIOMap . Map.fromAscListWith f

-- | /O(n)/. Build a map from a descending list in linear time with a combining function for equal keys.
-- /The precondition (input list is descending) is not checked./
--
-- > fromDescListWith (++) [(5,"a"), (5,"b"), (3,"b")] == fromList [(3, "b"), (5, "ba")]
-- > valid (fromDescListWith (++) [(5,"a"), (5,"b"), (3,"b")]) == True
-- > valid (fromDescListWith (++) [(5,"a"), (3,"b"), (5,"b")]) == False
--
-- @since 0.5.8

fromDescListWith :: (MonadIO m, Eq k) => (a -> a -> a) -> [(k,a)] -> m (IOMap k a)
fromDescListWith f = newIOMap . Map.fromDescListWith f

-- | /O(n)/. Build a map from an ascending list in linear time with a
-- combining function for equal keys.
-- /The precondition (input list is ascending) is not checked./
--
-- > let f k a1 a2 = (show k) ++ ":" ++ a1 ++ a2
-- > fromAscListWithKey f [(3,"b"), (5,"a"), (5,"b"), (5,"b")] == fromList [(3, "b"), (5, "5:b5:ba")]
-- > valid (fromAscListWithKey f [(3,"b"), (5,"a"), (5,"b"), (5,"b")]) == True
-- > valid (fromAscListWithKey f [(5,"a"), (3,"b"), (5,"b"), (5,"b")]) == False

fromAscListWithKey :: (MonadIO m, Eq k) => (k -> a -> a -> a) -> [(k,a)] -> m (IOMap k a)
fromAscListWithKey f = newIOMap . Map.fromAscListWithKey f

-- | /O(n)/. Build a map from a descending list in linear time with a
-- combining function for equal keys.
-- /The precondition (input list is descending) is not checked./
--
-- > let f k a1 a2 = (show k) ++ ":" ++ a1 ++ a2
-- > fromDescListWithKey f [(5,"a"), (5,"b"), (5,"b"), (3,"b")] == fromList [(3, "b"), (5, "5:b5:ba")]
-- > valid (fromDescListWithKey f [(5,"a"), (5,"b"), (5,"b"), (3,"b")]) == True
-- > valid (fromDescListWithKey f [(5,"a"), (3,"b"), (5,"b"), (5,"b")]) == False
fromDescListWithKey :: (MonadIO m, Eq k) => (k -> a -> a -> a) -> [(k,a)] -> m (IOMap k a)
fromDescListWithKey f = newIOMap . Map.fromDescListWithKey f

-- | /O(n)/. Build a map from an ascending list of distinct elements in linear time.
-- /The precondition is not checked./
--
-- > fromDistinctAscList [(3,"b"), (5,"a")] == fromList [(3, "b"), (5, "a")]
-- > valid (fromDistinctAscList [(3,"b"), (5,"a")])          == True
-- > valid (fromDistinctAscList [(3,"b"), (5,"a"), (5,"b")]) == False

-- For some reason, when 'singleton' is used in fromDistinctAscList or in
-- create, it is not inlined, so we inline it manually.
fromDistinctAscList :: MonadIO m => [(k,a)] -> m (IOMap k a)
fromDistinctAscList = newIOMap . Map.fromDistinctAscList

-- | /O(n)/. Build a map from a descending list of distinct elements in linear time.
-- /The precondition is not checked./
--
-- > fromDistinctDescList [(5,"a"), (3,"b")] == fromList [(3, "b"), (5, "a")]
-- > valid (fromDistinctDescList [(5,"a"), (3,"b")])          == True
-- > valid (fromDistinctDescList [(5,"a"), (5,"b"), (3,"b")]) == False
--
-- @since 0.5.8

-- For some reason, when 'singleton' is used in fromDistinctDescList or in
-- create, it is not inlined, so we inline it manually.
fromDistinctDescList :: MonadIO m => [(k,a)] -> m (IOMap k a)
fromDistinctDescList = newIOMap . Map.fromDistinctDescList
