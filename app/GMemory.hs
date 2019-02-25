{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module GMemory where

import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import Data.List (find)
import System.IO


newtype IORT s m a v = IORT
  { unIORT :: ReaderT (IORef [PtrR a]) m v
  } deriving (Functor, Applicative, Monad)

type SIO s = IORT s IO

type Bytes = Int

-- As reads and writes are not polymorphic step size not needed
-- If they were polymorphic `step_size = size_of x`; where x is instance of Storable
data PtrR a = PtrR { ptr        :: Ptr a
                   , ref_count  :: IORef Integer
                   , block_size :: Bytes
                   , free_size  :: Bytes
                   }

instance Eq (PtrR a) where
  (PtrR {ptr = ptr1}) == (PtrR {ptr = ptr2})
    = ptr1 == ptr2

free_ptr :: PtrR a -> IO ()
free_ptr (PtrR {ptr = ptr, ref_count = refcount}) = do
    hPutStrLn stderr $ "Freeing " ++ show ptr
    rc <- readIORef refcount
    assert (rc > 0) (return ())
    writeIORef refcount (pred rc)
    if rc > 1
       then hPutStrLn stderr "Aliased pointer wasn't closed"
       else free ptr

new_ptr :: Ptr a -> IO (PtrR a)
new_ptr ptr = do
  ioref <- newIORef 1
  return $ PtrR {ptr = ptr, ref_count = ioref, block_size = 0, free_size = 0}

eq_ptr :: Ptr a -> PtrR a -> Bool
eq_ptr ptr1 (PtrR {ptr = ptr2})= ptr1 == ptr2


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
      catch
        (free_ptr ptr)
        (\(e :: SomeException) -> do
           hPutStrLn stderr ("Exception while freeing: " ++ show e)
           return ())

newtype SPtr (m :: * -> *) a =
  SPtr (PtrR a)
  deriving Eq

liftSIO :: Monad m
        => IORT r m a v
        -> IORT s (IORT r m a) a v
liftSIO = IORT . lift


newMemBlock :: RMonadIO m => Bytes -> IORT s m a (SPtr (IORT s m a) a)
newMemBlock bytes = IORT r'
  where
    r' = do
      ptr <- liftRIO $ mallocBytes bytes
      liftRIO $ hPutStrLn stderr ("Allocating pointer : " ++ (show ptr))
      pointers <- ask
      ptr' <- liftRIO $ new_ptr ptr
      liftRIO $ modifyIORef pointers (ptr':)
      ioref <- liftRIO $ newIORef 1
      return $ SPtr (PtrR { ptr = ptr
                          , ref_count = ioref
                          , block_size = bytes
                          , free_size  = bytes})

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


-- mp is the parent region monad
copyPtr
  :: (RMonadIO m1, mp ~ IORT s' m1)
  => SPtr (IORT s m a) a
  -> IORT s (mp a) a (SPtr m a)
copyPtr (SPtr pr@(PtrR {ptr = p})) = IORT $ do
  pointers <- ask >>= liftRIO . readIORef
  let Just ptr@(PtrR {ref_count = refcount}) = find (eq_ptr p) pointers
  liftRIO $ modifyIORef refcount succ
  lift $ IORT (do -- in the parent monad
                  pointers <- ask
                  liftRIO $ modifyIORef pointers (ptr:))
  return (SPtr pr)


ptrThrow :: (Exception e, RMonadIO m) => e -> m a
ptrThrow = liftRIO . throwIO

ptrCatch :: (Exception e, RMonadIO m) => m a -> (e -> m a) -> m a
ptrCatch = rcatch

-- pointer arithmetic disallowed inside the IORT monad

{-

when reading something of type a
block_size >= size_of a

when writing something of type a
block_size >= free_size + size_of a
-}

readInt :: (MonadRaise m1 m2, RMonadIO m2) => SPtr m1 Int -> m2 Int
readInt (SPtr (PtrR {ptr = ptr, block_size = s, free_size = fs})) = do
  assert (s >= 8) (return ())
  liftRIO $ peek ptr

writeInt :: (MonadRaise m1 m2, RMonadIO m2) => SPtr m1 Int -> Int -> m2 ()
writeInt (SPtr (PtrR {ptr = ptr, block_size = s, free_size = fs})) n = do
  assert (s >= fs + sizeOf n) (return ())
  liftRIO $ poke ptr n

report :: (RMonadIO m) => String -> m ()
report = liftRIO . hPutStr stderr

boundaryOf :: (MonadRaise m1 m2, RMonadIO m2, Storable a) => SPtr m1 a -> m2 Bool
boundaryOf (SPtr a) = liftRIO (return True)



{-

resetRegion can be faked with fillBytes which is just a wrapper around `memset`

-}


foo = runSIO $ do
  x <- newMemBlock (0)
  writeInt x 5
  --m <- readInt x
  --return m



test3 = runSIO (do
  ptr1 <- newMemBlock 10
  ptr3 <- newRgn (test3_internal ptr1)
  till (boundaryOf ptr1)
       (readInt ptr1 >>= writeInt ptr3)
  report "test3 done")


till condition iteration = loop where
  loop = do b <- condition
            if b then return ()
                 else iteration >> loop

test3_internal ptr1 = do
  ptr2 <- newMemBlock 10
  int2 <- readInt ptr2
  ptr3 <- liftSIO (newMemBlock 10)                           -- In this case we statically know that we want ptr3 to outlive this region
  writeInt ptr3 int2
  till (liftM2 (||) (boundaryOf ptr2) (boundaryOf ptr1))
       (readInt ptr2 >>= writeInt ptr3 >>
        readInt ptr1 >>= writeInt ptr3)
  report "with ptr1 and ptr2"
  return ptr3


-- What if we statically don't know what we want to outlive?
-- Eg : we have 2 child regions RC1 and RC2 and depending on some computation we want to return either of the 2 regions and kill the other 

test5 = runSIO (do
  h <- newRgn (test5_internal 20)
  l <- readInt h
  report ("Continue with the older mem block: " ++ show l)
  report "test5 done")

test5_internal conf_fname = do
  hc <- newMemBlock conf_fname
  fname1 <- readInt hc
  fname2 <- readInt hc
  h1 <- newMemBlock fname1
  h2 <- newMemBlock fname2
  l1 <- readInt h1
  l2 <- readInt h2
  let (fname_old,h_old) | l1 < l2 = (fname2,h2)
                        | otherwise = (fname1,h1)
  report ("Older mem block: " ++ (show fname_old))
  copyPtr h_old -- prolong the life of that handle
