#lbsurv [![Build Status](https://travis-ci.org/lbraglia/lbsurv.svg)](https://travis-ci.org/lbraglia/lbsurv)

Some survival analysis utility functions from
[yapomif](http://github.com/lbraglia/yapomif), re-engineered following
these criteria:

- adopt hadley_s_style_for_naming things;
- move graphical functions to `ggplot2`;
- systematic/full-coverage unit testing;
- clean interfaces and remove unneeded parameters (after ~ a year of daily
  usage);
- R code cleaning  
- port code to C where useful

TODO:
- [ ] avg_surv
- [x] censor_at
- [ ] fup_completeness
- [ ] km
- [x] tteep