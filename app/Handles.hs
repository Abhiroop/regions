{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handles where

import Control.Exception
import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import System.IO

import Data.STRef

-- open 2 files: 1. config file 2. something else
-- read the name of a log file from 1
-- open log file and zip contents of 1 & 2 into log file
-- close the config file
-- copy content of 2 to log file
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
         let rr m = runReaderT m r
         in rbrace (rr before) (rr . after) (rr . during))
  rcatch m f =
    ReaderT
      (\r ->
         let rr m = runReaderT m r
         in rcatch (rr m) (\e -> runReaderT (f e) r))

instance RMonadIO m => RMonadIO (IORT s m) where
  rbrace before after during =
    IORT (rbrace (unIORT before) (unIORT . after) (unIORT . during))
  rcatch m f = IORT (unIORT m `rcatch` (unIORT . f))
  liftIO = IORT . liftIO

runSIO :: (forall s. SIO s v) -> IO v
runSIO m = rbrace (liftIO (newIORef [])) after (runReaderT $ unIORT m)
  where
    after handles = do
      hs <- readIORef handles
      mapM_ close hs
    close h = do
      hPutStrLn stderr ("Closing " ++ show h)
      catch
        (hClose h)
        (\(e :: IOException) -> do
           hPutStrLn stderr ("Exception while closing: " ++ show e)
           return ())

newtype SHandle (m :: * -> *) =
  SHandle Handle

newtype SubRegion r s =
  SubRegion (forall v . SIO r v -> SIO s v)
-- SubRegion r s is witness that r is the ancestor of s

newRgn :: (forall s . SubRegion r s -> SIO s v) -> SIO r v
newRgn body
  = IORT (do
             env_outer <- ask
             let witness (IORT m) =
                   liftIO (runReaderT m env_outer)
             liftIO (runSIO (body (SubRegion witness)))
         )
newSHandle :: FilePath -> IOMode -> SIO s (SHandle (SIO s))
newSHandle fname fmode = IORT r'
  where
    r' = do
      h <- liftIO $ openFile fname fmode
      handles <- ask
      liftIO $ modifyIORef handles (h:)
      return $ SHandle h


shThrow :: Exception e => e -> SIO s a
shThrow = liftIO . throwIO

shCatch :: Exception e => SIO s a -> (e -> SIO s a) -> SIO s a
shCatch = rcatch

shReport :: String -> SIO s ()
shReport = liftIO . hPutStr stderr

shGetLine :: SHandle (SIO s) -> SIO s String
shGetLine (SHandle h) = liftIO (hGetLine h)

shPutStrLn :: SHandle (SIO s) -> String -> SIO s ()
shPutStrLn (SHandle h) = liftIO . hPutStrLn h

shIsEOF :: SHandle (SIO s) -> SIO s Bool
shIsEOF (SHandle h) = liftIO (hIsEOF h)

foo =
  let a = newSTRef (15 :: Int)
          -- b = runST $ writeSTRef a 20
          -- c = runST $ readSTRef a
  in a --b `seq` c
