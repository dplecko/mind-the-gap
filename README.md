
This repository contains the code for reproducing the results of the paper "Mind the Gap: A Causal Perspective on Bias Amplification in Prediction \& Decision-Making".

Please check `R/zzz-deps.R` for the R-packages required to reproduce the results. Once these are installed (and assuming the availability of the MIMIC-IV dataset within the `ricu` package; see [here](https://physionet.org/content/mimiciv/2.2/) for access to MIMIC-IV data), the experiments can be reproduced as follows:

1. The MIMIC-IV experiment can be reproduced using the `mimic-census.R` script. The `mtg_wrap()` function is the main workhorse here.
2. The COMPAS experiment can be reproduced using the `compas-auditing.R`, which uses the `mind_the_gap()` functionality.
3. The Census 2018 experiment can be reproduced using the `mimic-census.R` script (similarly as for the MIMIC-IV dataset).
