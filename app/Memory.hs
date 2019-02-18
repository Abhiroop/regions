{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Memory where

import Foreign.Marshal.Alloc
import Foreign.Ptr

import Control.Exception
import Control.Monad.ST
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

newMemBlock :: Int -> SIO s a (SPtr (SIO s a) a)
newMemBlock bytes = IORT r'
  where
    r' = do
      liftIO $ hPutStrLn stderr ("Allocating " ++ (show bytes) ++ " bytes")
      ptr <- liftIO $ mallocBytes bytes
      handles <- ask
      liftIO $ modifyIORef handles (ptr:)
      return $ SPtr ptr


shThrow :: Exception e => e -> SIO s a v
shThrow = liftIO . throwIO

shCatch :: Exception e => SIO s a v -> (e -> SIO s a v) -> SIO s a v
shCatch = rcatch

foo = runSIO $ do
  x <- newMemBlock 10
  return ()
