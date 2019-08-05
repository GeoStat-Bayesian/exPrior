# exPrior

Welcome to the main repository for the exPrior R package.

## Purpose

The aim of this package is to provide practitioners of geostatistics a tool to derive prior distributions for Bayesian inference. The main features of the package are:

- generate prior distrubutions based on external, ie, ex-situ, data only, therefore meeting the likelihood principle,
- account for possible autocorrelation in these ex-situ data, therefore avoiding the problem of Pseudoreplication,
- account for available soft data, say, in the form of expert information on bounds and moments,
- comes prepackaged with the largest open-source database on hydrogeological parameters (batteries included), ect.

Recent papers have made the case that gesotatistics in subsurface hydrology is still falling short on available software tools (10.5194/hess-2018-290) and that Bayesian inference is the most relevant framework for susburface statistics (10.3389/feart.2019.00118). The exPrior package is therefore a timely addition to the R ecosystem of geostatistical tools.

## Installation

Currently, the bset way to install exPrior is through the github repository. Within R, the following commands are to be used

```R
library(devtools)
install_github("kcucchi/exPrior")
```

## Documentation

A publication detailing the algorithm can be found under the following DOI: 10.1016/j.advwatres.2019.02.003
