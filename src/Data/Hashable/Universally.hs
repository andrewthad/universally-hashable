{-# language BangPatterns #-}
{-# language TypeFamilies #-}

module Data.Hashable.Universally
  ( Hashable(..)
  ) where

import Data.Kind (Type)
import Data.Primitive (PrimArray,SmallArray)
import Data.IORef (IORef,writeIORef,readIORef,newIORef)
import Control.Monad (when)
import Control.Applicative (liftA2)
import Data.Bits ((.&.))
import qualified Data.Primitive as PM

class Hashable a where
  hash :: EntropyMonad m => EntropyType m -> a -> m Word

-- We use the multiply-shift scheme described on the wikipedia page
-- for universal hashing:
--
--   h(x) = a * x mod (2**w)
instance Hashable Word where
  hash e x = do
    arr <- values 2 True e
    pure ((PM.indexPrimArray arr 0 * makeOdd x) + PM.indexPrimArray arr 1)
    where
    makeOdd w = (w .&. (maxBound - 1)) + 1

instance Hashable a => Hashable (Maybe a) where
  hash e x = case x of
    Nothing -> do
      arr <- values 1 False e
      pure (PM.indexPrimArray arr 0)
    Just y -> do
      kids <- children 1 False e
      hash (PM.indexSmallArray kids 0) y

instance Hashable a => Hashable (SmallArray a) where
  hash e xs = do
    let sz = PM.sizeofSmallArray xs
    vals <- values (sz + 1) False e
    kids <- children sz False e
    let go !ix !acc = if ix < sz
          then do
            h <- hash (PM.indexSmallArray kids ix) (PM.indexSmallArray xs ix)
            go (ix + 1) (acc + h)
          else pure acc
    go 0 (PM.indexPrimArray vals sz)

instance (Hashable a, Hashable b) => Hashable (a,b) where
  hash e (a,b) = do
    arr <- children 2 True e
    x <- hash (PM.indexSmallArray arr 0) a
    y <- hash (PM.indexSmallArray arr 1) b
    pure (x + y)

class Monad m => EntropyMonad m where
  type family EntropyType m :: Type
  values :: Int -> Bool -> EntropyType m -> m (PrimArray Word)
  children :: Int -> Bool -> EntropyType m -> m (SmallArray (EntropyType m))

instance EntropyMonad Maybe where
  type EntropyType Maybe = Entropy
  values n isFixed (Entropy vals _) = if isFixed
    then Just vals
    else if PM.sizeofPrimArray vals >= n
      then Just vals
      else Nothing
  children n isFixed (Entropy _ kids) = if isFixed
    then Just kids
    else if PM.sizeofSmallArray kids >= n
      then Just kids
      else Nothing

instance EntropyMonad IO where
  type EntropyType IO = EntropyBuilder
  values n isFixed (EntropyBuilder valsRef _) = do
    vals <- readIORef valsRef
    let sz = PM.sizeofPrimArray vals
    if isFixed
      then do
        when (PM.sizeofPrimArray vals /= sz) $ do
          fail "fixed entropy values size does not match"
        pure vals
      else if sz >= n
        then pure vals
        else do
          mutVals' <- PM.newPrimArray n
          PM.copyPrimArray mutVals' 0 vals 0 sz
          -- TODO: fill in the new area with random data
          vals' <- PM.unsafeFreezePrimArray mutVals'
          writeIORef valsRef vals'
          pure vals'
          -- _ <- PM.newSmallArray n 
  children n isFixed (EntropyBuilder _ kidsRef) = do
    kids <- readIORef kidsRef
    let sz = PM.sizeofSmallArray kids
    if isFixed
      then do
        when (PM.sizeofSmallArray kids /= sz) $ do
          fail "fixed entropy values size does not match"
        pure kids
      else if sz >= n
        then pure kids
        else do
          mutKids' <- PM.newSmallArray n errorThunk
          PM.copySmallArray mutKids' 0 kids 0 sz
          let go !ix = if ix < n
                then do
                  PM.writeSmallArray mutKids' ix =<< liftA2 EntropyBuilder (newIORef mempty) (newIORef mempty)
                  go (ix + 1)
                else pure ()
          go sz
          kids' <- PM.unsafeFreezeSmallArray mutKids'
          writeIORef kidsRef kids'
          pure kids'

{-# NOINLINE errorThunk #-}
errorThunk :: a
errorThunk = error "Data.Hashable.Universally: implementation error"

data Entropy = Entropy !(PrimArray Word) !(SmallArray Entropy)
data EntropyBuilder = EntropyBuilder
  !(IORef (PrimArray Word))
  !(IORef (SmallArray EntropyBuilder))

