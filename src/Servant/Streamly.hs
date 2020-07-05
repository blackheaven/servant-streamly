{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | This module exports 'ToSourceIO' and 'FromSourceIO' for all 'IsStream' instances.
module Servant.Streamly
  ( StreamlyToSourceIO(..)
  )
where

import           Control.Monad.IO.Class         ( MonadIO(..)
                                                , liftIO
                                                )
import           Control.Monad.Trans.Resource   ( ResourceT
                                                , runResourceT
                                                )
import qualified Streamly
import qualified Streamly.Prelude              as Streamly
import qualified Servant.API.Stream            as Servant
import qualified Servant.Types.SourceT         as Servant

-- | Helper class to implement @'ToSourceIO' 'IsStream'@ instance
-- for various monads.
class StreamlyToSourceIO m where
    streamlyToSourceIO :: Streamly.IsStream t => t m a -> Servant.SourceIO a

instance StreamlyToSourceIO IO where
  streamlyToSourceIO stream = Servant.SourceT
    ($ transform $ Streamly.adapt stream)
   where
    transform = Servant.Effect . Streamly.foldr Servant.Yield Servant.Stop

instance StreamlyToSourceIO (ResourceT IO) where
  streamlyToSourceIO stream = Servant.SourceT
    ($ transform $ Streamly.adapt stream)
   where
    transform =
      Servant.Effect . runResourceT . Streamly.foldr Servant.Yield Servant.Stop

instance (StreamlyToSourceIO m, Streamly.IsStream t) => Servant.ToSourceIO a (t m a) where
  toSourceIO = streamlyToSourceIO

instance (Streamly.IsStream t) => Servant.FromSourceIO a (t IO a) where
  fromSourceIO src =
    Streamly.concatMapM id $ Streamly.yield $ Servant.unSourceT src go
   where
    go :: Streamly.IsStream t => Servant.StepT IO a -> IO (t IO a)
    go step = case step of
      Servant.Stop             -> return Streamly.nil
      Servant.Error e          -> return $ Streamly.yieldM $ error e
      Servant.Skip  n          -> go n
      Servant.Yield x nextStep -> Streamly.cons x <$> go nextStep
      Servant.Effect nextStep  -> nextStep >>= go
  -- {-# SPECIALIZE INLINE fromSourceIO :: Streamly.IsStream t => Servant.SourceIO a -> t IO a #-}

instance (Streamly.IsStream t) => Servant.FromSourceIO a (t (ResourceT IO) a) where
  fromSourceIO src =
    Streamly.concatMapM id $ Streamly.yield $ liftIO $ Servant.unSourceT src go
   where
    go :: Streamly.IsStream t => Servant.StepT IO a -> IO (t (ResourceT IO) a)
    go step = case step of
      Servant.Stop             -> return Streamly.nil
      Servant.Error e          -> return $ Streamly.yieldM $ error e
      Servant.Skip  n          -> go n
      Servant.Yield x nextStep -> Streamly.cons x <$> go nextStep
      Servant.Effect nextStep  -> nextStep >>= go
