# fixed-timestep

Pure Haskell library to run an action repeatedly, a specific amount of times per second.

Internal timekeeping is done in [flicks](https://github.com/OculusVR/Flicks#README), a
unit of time equal to precisely 1/705600000 of a second.

Basic usage:

```haskell
λ> import Data.Time.Clock
λ> import TimeStep
λ> repeatedly 1 (print =<< getCurrentTime)
2019-03-13 21:14:36.826248 UTC
2019-03-13 21:14:37.8282926 UTC
2019-03-13 21:14:38.8327692 UTC
2019-03-13 21:14:39.8359494 UTC
2019-03-13 21:14:40.8374415 UTC
^CInterrupted.
```

Using `async`, you can repeat multiple actions independently, and also cancel them:

```haskell
λ> import Data.Time.Clock
λ> import Control.Concurrent.Asyncx
λ> import TimeStep
λ> printer <- asyncRepeatedly 1 (print =<< getCurrentTime)
2019-03-13 21:21:27.5228834 UTC
2019-03-13 21:21:28.523892 UTC
2019-03-13 21:21:29.5283634 UTC
2019-03-13 21:21:30.5313565 UTC
2019-03-13 21:21:31.535904 UTC
2019-03-13 21:21:32.5373828 UTC
2019-03-13 21:21:33.5393834 UTC
2019-03-13 21:21:34.5408811 UTC
cancel printer
```