# Revision history for fixed-timestep

## 0.2.0.1 -- 2019-03-25

* Improve docs
  * Clarify that frequencies are given in units of Hertz.

## 0.2.0.0 -- 2019-03-25

* Rename `Time.Flick.secnd` to `Time.Flick.oneSecond`
  * This pleases the Orthography Gods.
* Move `TimeStep` module to `Time.Repeatedly`
  * It makes more sense to keep all modules under one namespace.

## 0.1.0.0 -- 2019-03-13

* Released. Basic timekeeping in units of flicks, and
  functions for repeatedly running actions.
