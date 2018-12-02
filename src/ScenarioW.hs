{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module ScenarioW(
  ScenarioT(),
  Scenario,
  runScenarioT,

  section, evaluateWHNF, evaluateNF,

  benchScenario, benchScenarioT,
  MonadBench(..),
) where

import           Control.Monad.IO.Class ()
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer

import           Data.Functor.Identity

import           Control.DeepSeq
import qualified Control.Exception      (evaluate)

import           Gauge.Benchmark

type Label = String

newtype ScenarioT m a = ScT { runScT :: WriterT [Benchmark] m a }
    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

deriving instance (MonadReader r m) => MonadReader r (ScenarioT m)
deriving instance (MonadState s m) => MonadState s (ScenarioT m)

type Scenario = ScenarioT Identity

runScenarioT :: (Monad m) => ScenarioT m a -> m a
runScenarioT = fmap fst . benchScenarioT

section :: (MonadBench m) => Label -> a -> (a -> ScenarioT m b) -> ScenarioT m b
section label a k = ScT $ WriterT $
  do benchmarkable <- detach id (runScenarioT . k) a
     let ScT (WriterT m) = k a
     (b, benches) <- m
     return (b, bench label benchmarkable : benches)

evaluateWHNF :: (MonadBench m) => a -> ScenarioT m a
evaluateWHNF a = ScT $ evaluate a

evaluateNF :: (MonadBench m, NFData a) => a -> ScenarioT m a
evaluateNF a = ScT $ evaluate (rnf a) >> return a

benchScenario :: Scenario a -> (a, [Benchmark])
benchScenario = runIdentity . benchScenarioT

benchScenarioT :: ScenarioT m a -> m (a, [Benchmark])
benchScenarioT = runWriterT . runScT

-- | Monads which computation can be turned into @Benchmarkable@.
class Monad m => MonadBench m where
  -- | @evaluate@ creates checkpoint in monadic computation.
  --   All actions after @evaluate a@ must be executed after @a@ is
  --   forced to WHNF(weak head normal form).
  evaluate :: a -> m a
  -- | @detach r f x@ cuts out monadic computation @f x@
  --   starting from current context as 'Benchmarkable'.
  --
  --   Aside from @IO@, @detach@ should not change the context.
  --   For example, @MonadBench (State s)@ instance returns
  --   @Benchmarkable@ that uses current state, but never changes
  --   the state outside of the benchmark.
  --
  --   Returned benchmark includes forcing @r@ applied to
  --   returned value of @f x@.
  --   If it should be forced to WHNF, @r = id@. Or if it
  --   is needed to be forced deeply, @r = rnf@.
  detach :: (b -> c) -> (a -> m b) -> a -> m Benchmarkable

instance MonadBench Identity where
  evaluate a = a `seq` Identity a
  detach r f x =
    Identity $ whnf (r . runIdentity . f) x

instance MonadBench IO where
  evaluate = Control.Exception.evaluate
  detach r f x = return (whnfIO (fmap r (f x)))

instance (MonadBench m) => MonadBench (ReaderT e m) where
  evaluate = lift . evaluate
  detach r f x =
    ReaderT $ \e ->
      detach r (\x' -> runReaderT (f x') e) x

instance (MonadBench m) => MonadBench (StateT s m) where
  evaluate = lift . evaluate
  detach r f x =
    StateT $ \s ->
      do b <- detach (r . fst) (\x' -> runStateT (f x') s) x
         return (b, s)

instance (Monoid s, MonadBench m) => MonadBench (WriterT s m) where
  evaluate = lift . evaluate
  detach r f x =
    WriterT $
      do bench' <- detach (r . fst) (runWriterT . f) x
         return (bench', mempty)
