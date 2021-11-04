# version 0.4.0 - November 14, 2021

- cross validation (in `crossval_ts`) on a percentage of the original 
dataset (training/testing dataset)
- evaluation on a __validation set__ (function `eval_ts`, must have the 
exact same `p` as `crossval_ts` to avoid overlapping slices)

# version 0.3.2

- Fix bug in `show_progress` (0% displayed when `show_progress==FALSE`)
