# scenario-measurement
Benchmark your usage scenario.

## Introduction

`scenario-measurement` is a library to help you write your benchmarks,
and built upon [gauge](http://hackage.haskell.org/package/gauge).

This library features a monad transformer: `ScenarioT`. The following code shows the core functions to deal with `ScenarioT`.

``` haskell
newtype ScenarioT m a = ...

-- | Create a section to be benchmarked
section :: (Functor m) => Label -> a -> (a -> ScenarioT m b) -> ScenarioT m b

-- | Argument will be forced to WHNF past this action.
evaluateWHNF :: (Functor m) => a -> ScenarioT m a

-- | Argument will be forced to NF past this action.
evaluateNF :: (Functor m, NFData a) => a -> ScenarioT m a

-- | Run entire scenario once, while collecting each section of scenatio as Benchmark.
benchScenarioT :: (MonadBench m) => ScenarioT m a -> m (a, [Benchmark])
```

Here, `MonadBench` is a type class for `Monad`s which computations can be turned into `Benchmarkable`.

``` haskell
-- | Monads which computations can be turned into @Benchmarkable@.
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
```

Currently, `MonadBench` has these instances.

``` haskell
instance MonadBench Identity
instance MonadBench IO
instance (MonadBench m) => MonadBench (ReaderT e m)
instance (MonadBench m) => MonadBench (StateT s m)
instance (Monoid s, MonadBench m) => MonadBench (WriterT s m)
```

## Example Usage

Consider benchmarking following function and each step of it. You will create six benchmarks here -
one for entire function, `readFile`, `preprocess`, `computation1`, `computation2`, and `writeFile`.
To provide inputs for their benchmark, you also will use `env` function to setup inputs for each functions. This sometimes is cumbersome to do.

``` haskell
processFile :: FilePath -> FilePath -> IO ()
processFile targetFile outputFile = do
  content <- readFile targetFile
  let content'    = preprocess content
      middleValue = computation1 content'
      finalValue  = computation2 middleValue
  liftIO $ writeFile outputFile (show finalValue)
```

Using this library, Setting up of inputs and defining benchmarks can be done simultaneously in one `do` notation.

``` haskell
processFileScenario :: FilePath -> FilePath -> ScenarioT IO ()
processFileScenario targetFile outputFile =
  section "all" () $ \_ -> do
    content <- section "read" () $ \_ ->
      liftIO (readFile targetFile) >>= evaluateNF
    content'<- section "preprocess" content (evaluateNF . preprocess)
    middleValue <- section "computation1" content' (evaluateNF . computation1)
    finalValue  <- section "computation2" middleValue (evaluateNF . computation2)
    section "write" finalValue (liftIO . writeFile outputFile . show)
```

Defined benchmarks can be extracted using `benchScenarioT`.

``` haskell
main :: IO ()
main =
  do (_, benchesFoo) <- benchScenarioT (processFileScenario "foo.txt" "foo.out")
     (_, benchesBar) <- benchScenarioT (processFileScenario "bar.txt" "bar.out")
     defaultMain
       [ bgroup "foo" benchesFoo
       , bgroup "bar" benchesBar
       ]
```
