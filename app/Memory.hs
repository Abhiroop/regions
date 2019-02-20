{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Memory where

import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

import Control.Exception
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import System.IO


newtype IORT s m a v = IORT
  { unIORT :: ReaderT (IORef [Ptr a]) m v
  } deriving (Functor, Applicative, Monad)

type SIO s = IORT s IO

class Monad m => RMonadIO m where
  liftIO :: IO a -> m a
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
  liftIO = id
  rbrace = bracket
  rcatch = catch

instance RMonadIO m => RMonadIO (ReaderT r m) where
  liftIO = lift . liftIO
  rbrace before after during =
    ReaderT
      (\r ->
         let rr m = runReaderT m r
         in rbrace (rr before) (rr . after) (rr . during))
  rcatch m f =
    ReaderT
      (\r ->
         let rr m = runReaderT m r
         in rcatch (rr m) (\e -> runReaderT (f e) r))

instance RMonadIO m => RMonadIO (IORT s m a) where
  rbrace before after during =
    IORT (rbrace (unIORT before) (unIORT . after) (unIORT . during))
  rcatch m f = IORT (unIORT m `rcatch` (unIORT . f))
  liftIO = IORT . liftIO

runSIO :: (forall s. SIO s a v) -> IO v
runSIO m = rbrace (liftIO (newIORef [])) after (runReaderT $ unIORT m)
  where
    after pointers = do
      ptrs <- readIORef pointers
      mapM_ freeMem ptrs
    freeMem ptr = do
      hPutStrLn stderr ("Freeing " ++ show ptr)
      catch
        (free ptr)
        (\(e :: IOException) -> do
           hPutStrLn stderr ("Exception while freeing: " ++ show e)
           return ())

newtype SPtr (m :: * -> *) a =
  SPtr (Ptr a)
  deriving Eq

newtype SubRegion r s a =
  SubRegion (forall v . SIO r a v -> SIO s a v)

-- SubRegion r s is witness that region r is the ancestor of region s in a region of type a

newRgn :: (forall s . SubRegion r s a -> SIO s a v) -> SIO r a v
newRgn body
  = IORT $ do
  env_outer <- ask
  let witness (IORT m) = liftIO (runReaderT m env_outer)
  liftIO (runSIO (body (SubRegion witness)))

newMemBlock :: Int -> SIO s a (SPtr (SIO s a) a)
newMemBlock bytes = IORT r'
  where
    r' = do
      ptr <- liftIO $ mallocBytes bytes
      liftIO $ hPutStrLn stderr ("Allocating pointer : " ++ (show ptr))
      handles <- ask
      liftIO $ modifyIORef handles (ptr:)
      return $ SPtr ptr


ptrThrow :: Exception e => e -> SIO s a v
ptrThrow = liftIO . throwIO

ptrCatch :: Exception e => SIO s a v -> (e -> SIO s a v) -> SIO s a v
ptrCatch = rcatch

readInt :: (SPtr (SIO s Int) Int) -> SIO s Int Int
readInt (SPtr ptr) = liftIO $ peek ptr

writeInt :: (SPtr (SIO s Int) Int) -> Int -> SIO s Int ()
writeInt (SPtr ptr) = liftIO . poke ptr

report :: String -> SIO s Int ()
report = liftIO . hPutStr stderr

foo = runSIO $ do
  x <- newMemBlock 10
  writeInt x 50
  j <- readInt x
  --report (show j)
  return j

newNaiveReg :: (forall s . SIO s a v) -> SIO r a v
newNaiveReg m = liftIO $ runSIO m

-- bar = runSIO $ do
--   x <- newMemBlock 10
--   -- some computation in the memory
--   m <- readInt x
--   let op = newMemBlock 5
--   res <- newNaiveReg (bar1 m op)
--   return ()

-- bar1 :: Int -> SIO s Int (SPtr (SIO s Int) Int) -> SIO s Int Int
-- bar1 m op = do
--   z <- newMemBlock 10
--   h <- op
--   --l1 <- readInt x
--   j <- readInt z
--   k <- readInt h
--   return (m + j + k)

bar = runSIO $ do
  x <- newMemBlock 10
  -- some computation in the memory
  m <- readInt x
  let op = newMemBlock 5
  res <- newRgn (\(SubRegion witness) -> do
                         z <- newMemBlock 10
                         h <- op
                         l1 <- witness $ readInt x
                         j <- readInt z
                         k <- readInt h
                         return (m + j + k)
                     )
  return ()


