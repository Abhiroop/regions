{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module SHandles where

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import System.IO

import Data.STRef

newtype IORT s m v = IORT
  { unIORT :: ReaderT (IORef [Handle]) m v
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

-- In the older GHCs something the IOError constructor was exposed which meant some handles could be leaked and we could use some kind of
-- sanitizeExc e =
--     maybe e (\e -> IOException e{ioe_handle = Nothing}) $ ioErrors e
-- to clean the exposed handles. This has been fixed in newer GHCs. Check IOException
instance RMonadIO IO where
  liftIO = id
  rbrace = bracket
  rcatch = catch

instance RMonadIO m => RMonadIO (ReaderT r m) where
  liftIO = lift . liftIO
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

instance RMonadIO m => RMonadIO (IORT s m) where
  rbrace before after during =
    IORT (rbrace (unIORT before) (unIORT . after) (unIORT . during))
  rcatch m f = IORT (unIORT m `rcatch` (unIORT . f))
  liftIO = IORT . liftIO


newtype SHandle (m :: * -> *) =
  SHandle Handle

liftSIO :: Monad m
        => IORT r m a
        -> IORT s (IORT r m) a
liftSIO = IORT . lift



newRgn :: RMonadIO m
       => (forall s . IORT s m v)
       ->                    m v
newRgn m
  = rbrace (liftIO (newIORef [])) after (runReaderT $ unIORT m)
  where
    after handles = liftIO $ do
      hs <- readIORef handles
      mapM_ close hs
    close h = do
      hPutStrLn stderr ("Closing " ++ show h)
      catch
        (hClose h)
        (\(e :: SomeException) -> do
           hPutStrLn stderr ("Exception while closing: " ++ show e)
           return ())

runSIO :: (forall s. SIO s v) -> IO v
runSIO = newRgn



newSHandle :: RMonadIO m => FilePath -> IOMode -> IORT s m (SHandle (IORT s m) )
newSHandle fname fmode = IORT r'
  where
    r' = do
      h <- liftIO $ openFile fname fmode
      handles <- ask
      liftIO $ modifyIORef handles (h:)
      return $ SHandle h




-- mimics implicit region subtyping
-- MonadRaise m1 m2 holds when either 
--   - m2 is the same as m1, or
--   - m2 is the sequence of one or more IORT applied to m1.
class (Monad m1, Monad m2) => MonadRaise m1 m2 where
  lifts :: m1 a -> m2 a

instance {-# OVERLAPPABLE #-} Monad m => MonadRaise m m where
  lifts = id

instance {-# OVERLAPPABLE #-} (Monad m1, Monad m2, m2 ~ (IORT s x), MonadRaise m1 x) => MonadRaise m1 m2 where
  lifts = IORT . lift . lifts


shThrow :: (Exception e, RMonadIO m) => e -> m a
shThrow = liftIO . throwIO

shCatch :: (Exception e, RMonadIO m) => m a -> (e -> m a) -> m a
shCatch = rcatch

shReport :: (RMonadIO m) => String -> m ()
shReport = liftIO . hPutStr stderr

shGetLine :: (MonadRaise m1 m2, RMonadIO m2) => SHandle m1 -> m2 String
shGetLine (SHandle h) = liftIO (hGetLine h)

shPutStrLn :: (MonadRaise m1 m2, RMonadIO m2) => SHandle m1 -> String -> m2 ()
shPutStrLn (SHandle h) = liftIO . hPutStrLn h

shIsEOF :: (MonadRaise m1 m2, RMonadIO m2) => SHandle m1 -> m2 Bool
shIsEOF (SHandle h) = liftIO (hIsEOF h)


test3 = runSIO (do
  h1 <- newSHandle "/tmp/SafeHandles.hs" ReadMode
  h3 <- test3_internal h1
  till (shIsEOF h1)
       (shGetLine h1 >>= shPutStrLn h3)
  shReport "test3 done")


till condition iteration = loop where
  loop = do b <- condition
            if b then return ()
                 else iteration >> loop

test3_internal h1 = do
  h2 <- newSHandle "/tmp/ex-file.conf" ReadMode
  fname <- shGetLine h2
  h3 <- newSHandle fname WriteMode
  shPutStrLn h3 fname
  till (liftM2 (||) (shIsEOF h2) (shIsEOF h1))
       (shGetLine h2 >>= shPutStrLn h3 >>
        shGetLine h1 >>= shPutStrLn h3)
  shReport "Finished zipping h1 and h2"
  return h3
