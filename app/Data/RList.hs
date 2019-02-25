{-# LANGUAGE RankNTypes #-}
module Data.RList where

import GMemory
import Foreign.Storable
import Foreign.Ptr


--type RList m a = (RMonadIO m) => SPtr m a

mapR :: (RMonadIO m, Storable a)
     => (a -> a)
     -> SPtr m a
     -> IO (SPtr m a)
mapR f (SPtr (PtrR {ptr = ptr, block_size = s, ref_count = rc, free_size = fs })) = do
  let h = ptr
  go h ptr 0
  where
    go p1 p pos
      | pos >= 10 = return (SPtr (PtrR {ptr = p1, block_size = s, ref_count = rc, free_size = fs}))
      | otherwise = do
          x <- peek p
          poke p (f x)
          go p1 (plusPtr p 8) (pos + 1)
