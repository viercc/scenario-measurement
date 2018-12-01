{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
module Scenario(
  ScenarioT(),
  Scenario,
  runScenarioT,

  section, evaluateWHNF, evaluateNF,

  benchScenario, benchScenarioT,
  MonadBench(..),
) where

import           Control.Monad
import           Control.Monad.IO.Class ()
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer

import           Data.Functor.Identity

import           Control.DeepSeq
import qualified Control.Exception      (evaluate)

import           Gauge.Benchmark

type Label = String

data ScenarioF r =
    BeginSection Label r
  | EndSection r
  | Evaluate () r
  deriving (Show, Functor)

newtype ScenarioT m a = ScT
  { runScT :: forall r.
         (forall x. ScenarioF x -> (x -> r) -> r)
      -> (m r -> r)
      -> (a -> r)
      -> r
  }
type Scenario = ScenarioT Identity

hoistStT :: forall m n b. (Monad m, Monad n) =>
  (forall a. m a -> n a) -> ScenarioT m b -> ScenarioT n b
hoistStT phi (ScT m) = ScT $ \act wrap ret -> m act (wrap . phi) ret

instance Functor (ScenarioT m) where
  fmap f ma = ScT $ \act wrap ret ->
    runScT ma act wrap (ret . f)

instance Applicative (ScenarioT m) where
  pure = return
  (<*>) = ap

instance Monad (ScenarioT m) where
  return a = ScT $ \_ _ ret -> ret a
  ma >>= k = ScT $ \act wrap ret ->
    runScT ma act wrap $ \a ->
      runScT (k a) act wrap ret

instance MonadTrans ScenarioT where
  lift ma = ScT $ \_ wrap ret -> wrap (fmap ret ma)

instance (MonadIO m) => MonadIO (ScenarioT m) where
  liftIO = lift . liftIO

instance (MonadReader e m) => MonadReader e (ScenarioT m) where
  ask = lift ask
  local f = hoistStT (local f)

instance (MonadState s m) => MonadState s (ScenarioT m) where
  get = lift get
  put = lift . put
  state = lift . state

runScenarioT :: (Monad m) => ScenarioT m a -> m a
runScenarioT ma = runScT ma handle join return
  where handle (BeginSection _ r) k = k r
        handle (EndSection r) k     = k r
        handle (Evaluate thunk r) k = thunk `seq` k r

inject :: ScenarioF a -> ScenarioT m a
inject fa = ScT $ \act _ ret -> act fa ret

section :: (Functor m) => Label -> a -> (a -> ScenarioT m b) -> ScenarioT m b
section label a k =
  do a' <- inject (BeginSection label a)
     b <- k a'
     inject (EndSection b)

evaluateWHNF :: (Functor m) => a -> ScenarioT m a
evaluateWHNF a = inject $ Evaluate (a `seq` ()) a

evaluateNF :: (Functor m, NFData a) => a -> ScenarioT m a
evaluateNF a = inject $ Evaluate (rnf a) a

data RScenarioT' m a where
  Pure :: a -> RScenarioT' m a
  ActBind :: ScenarioF a -> (a -> RScenarioT m b) -> RScenarioT' m b

newtype RScenarioT m a = RS (m (RScenarioT' m a))

reify :: (Monad m) => ScenarioT m a -> RScenarioT m a
reify ma =
  RS $ runScT ma (\fa k -> return (ActBind fa (RS . k))) join (return . Pure)

benchScenario :: Scenario a -> (a, [Benchmark])
benchScenario = go . reify
  where
    go (RS (Identity (Pure a))) = (a, [])
    go (RS (Identity (ActBind fa k))) = case fa of
      BeginSection label a ->
        let (b, benches) = go (k a)
            newBench = bench label (whnf (runSectionPure . k) a)
        in (b, newBench : benches)
      EndSection a     -> go (k a)
      Evaluate thunk a -> thunk `seq` go (k a)

runSectionPure :: RScenarioT Identity a -> RScenarioT Identity a
runSectionPure = go (0 :: Int)
  where
    go !_ (RS (Identity (Pure _))) = error "No matching EndSection"
    go !n (RS (Identity (ActBind fa k))) = case fa of
      BeginSection _ a -> go (n+1) (k a)
      EndSection a | n == 0    -> k a
                   | otherwise -> go (n-1) (k a)
      Evaluate thunk a | n == 0    -> thunk `seq` go n (k a)
                       | otherwise -> go n (k a)

benchScenarioT :: (MonadBench m) => ScenarioT m a -> m (a, [Benchmark])
benchScenarioT ma =
  do (a, benches) <- go [] (reify ma)
     return (a, reverse benches)
  where
    go accum (RS mRS) =
      do rs <- mRS
         case rs of
           Pure a -> return (a, accum)
           ActBind fa k -> case fa of
             BeginSection label a ->
               do newBench <- detach id (runSectionT . k) a
                  go (bench label newBench : accum) (k a)
             EndSection a -> go accum (k a)
             Evaluate thunk a -> evaluate thunk >> go accum (k a)

runSectionT :: MonadBench m => RScenarioT m a -> m (RScenarioT m a)
runSectionT = go (0 :: Int)
  where
    go !n (RS mRS) =
      do rs <- mRS
         case rs of
           Pure _ -> error "No matching EndSection"
           ActBind fa k -> case fa of
             BeginSection _ a -> go (n+1) (k a)
             EndSection a | n == 0    -> return (k a)
                          | otherwise -> go (n-1) (k a)
             Evaluate thunk a | n == 0    -> evaluate thunk >> go n (k a)
                              | otherwise -> go n (k a)

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
