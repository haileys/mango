module Mango.MutableMap (
    MutableMap,
    fromList,
    member,
    lookup,
    insert,
) where

import Prelude hiding (lookup)
import Data.IORef
import qualified Data.Map as Map

newtype MutableMap k v = MutableMap (IORef (Map.Map k v))

fromList :: Ord k => [(k,v)] -> IO (MutableMap k v)
fromList elems = do
    ref <- newIORef $ Map.fromList elems
    return (MutableMap ref)

member :: Ord k => k -> MutableMap k a -> IO Bool
member k (MutableMap ref) = do
    m <- readIORef ref
    return $ Map.member k m

lookup :: Ord k => k -> MutableMap k a -> IO (Maybe a)
lookup k (MutableMap ref) = do
    m <- readIORef ref
    return $ Map.lookup k m

insert :: Ord k => k -> a -> MutableMap k a -> IO ()
insert k v (MutableMap ref) = do
    m <- readIORef ref
    writeIORef ref $ Map.insert k v m