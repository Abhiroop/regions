{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module SMemory where

import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import System.IO


newtype IORT s m a v = IORT
  { unIORT :: ReaderT (IORef [Ptr a]) m v
  } deriving (Functor, Applicative, Monad)

type SIO s = IORT s IO

class Monad m => RMonadIO m where
  liftRIO :: IO a -> m a
  rcatch ::
       Exception e
    => m a -- computation to run
    -> (e -> m a) -- handler to call if exception is raised
    -> m a
  rbrace ::
       m a -- acquire resource
    -> (a -> m b) -- release resource
    -> (a -> m c) -- run in between
    -> m c

instance RMonadIO IO where
  liftRIO = id
  rbrace = bracket
  rcatch = catch

instance RMonadIO m => RMonadIO (ReaderT r m) where
  liftRIO = lift . liftRIO
  rbrace before after during =
    ReaderT
      (\r ->
         let rr :: RMonadIO m => ReaderT r m a -> m a
             rr m = runReaderT m r
         in rbrace (rr before) (rr . after) (rr . during))
  rcatch m f =
    ReaderT
      (\r ->
         let rr :: RMonadIO m => ReaderT r m a -> m a
             rr m = runReaderT m r
         in rcatch (rr m) (\e -> runReaderT (f e) r))

instance RMonadIO m => RMonadIO (IORT s m a) where
  rbrace before after during =
    IORT (rbrace (unIORT before) (unIORT . after) (unIORT . during))
  rcatch m f = IORT (unIORT m `rcatch` (unIORT . f))
  liftRIO = IORT . liftRIO

runSIO :: (forall s. SIO s a v) -> IO v
runSIO = newRgn


newRgn :: (RMonadIO m)
       => (forall s . IORT s m a v)
       -> m v
newRgn body
  = rbrace (liftRIO (newIORef [])) after (runReaderT $ unIORT body)
  where
    after pointers = liftRIO $ do
      ptrs <- readIORef pointers
      mapM_ freeMem ptrs
    freeMem ptr = do
      hPutStrLn stderr ("Freeing " ++ show ptr)
      catch
        (free ptr)
        (\(e :: SomeException) -> do
           hPutStrLn stderr ("Exception while freeing: " ++ show e)
           return ())

newtype SPtr (m :: * -> *) a =
  SPtr (Ptr a)
  deriving Eq

liftSIO :: Monad m
        => IORT r m a v
        -> IORT s (IORT r m a) a v
liftSIO = IORT . lift

type Bytes = Int

newMemBlock :: RMonadIO m => Bytes -> IORT s m a (SPtr (IORT s m a) a)
newMemBlock bytes = IORT r'
  where
    r' = do
      ptr <- liftRIO $ mallocBytes bytes
      liftRIO $ hPutStrLn stderr ("Allocating pointer : " ++ (show ptr))
      handles <- ask
      liftRIO $ modifyIORef handles (ptr:)
      return $ SPtr ptr

-- mimics implicit region subtyping
-- MonadRaise m1 m2 holds when either 
--   - m2 is the same as m1, or
--   - m2 is the sequence of one or more IORT applied to m1.
class (Monad m1, Monad m2) => MonadRaise m1 m2 where
  lifts :: m1 a -> m2 a

instance {-# OVERLAPPABLE #-} Monad m => MonadRaise m m where
  lifts = id

instance {-# OVERLAPPABLE #-} (Monad m1, Monad m2, m2 ~ (IORT s x a), MonadRaise m1 x) => MonadRaise m1 m2 where
  lifts = IORT . lift . lifts


ptrThrow :: (Exception e, RMonadIO m) => e -> m a
ptrThrow = liftRIO . throwIO

ptrCatch :: (Exception e, RMonadIO m) => m a -> (e -> m a) -> m a
ptrCatch = rcatch

readMem :: (MonadRaise m1 m2, RMonadIO m2, Storable a) => SPtr m1 a -> m2 a
readMem (SPtr ptr) = liftRIO $ peek ptr

writeMem :: (MonadRaise m1 m2, RMonadIO m2, Storable a) => SPtr m1 a -> a -> m2 ()
writeMem (SPtr ptr) = liftRIO . poke ptr

report :: (RMonadIO m) => String -> m ()
report = liftRIO . hPutStr stderr

boundaryOf :: (MonadRaise m1 m2, RMonadIO m2) => SPtr m1 Int -> m2 Bool
boundaryOf (SPtr a) = liftRIO (return True)



{-

resetRegion can be faked with fillBytes which is just a wrapper around `memset`

-}




test3 = runSIO (do
  ptr1 <- newMemBlock 10
  ptr3 <- newRgn (test3_internal ptr1)
  till (boundaryOf ptr1)
       (readMem ptr1 >>= writeMem ptr3)
  report "test3 done")


till condition iteration = loop where
  loop = do b <- condition
            if b then return ()
                 else iteration >> loop

test3_internal ptr1 = do
  ptr2 <- newMemBlock 10
  int2 <- readMem ptr2
  ptr3 <- liftSIO (newMemBlock 10)                           -- In this case we statically know that we want ptr3 to outlive this region
  writeMem ptr3 int2
  till (liftM2 (||) (boundaryOf ptr2) (boundaryOf ptr1))
       (readMem ptr2 >>= writeMem ptr3 >>
        readMem ptr1 >>= writeMem ptr3)
  report "with ptr1 and ptr2"
  return ptr3


-- What if we statically don't know what we want to outlive?
-- Eg : we have 2 child regions RC1 and RC2 and depending on some computation we want to return either of the 2 regions and kill the other 
