# exPrior

Welcome to the main repository for the exPrior R package.

## Purpose

The aim of this package is to provide practitioners of geostatistics a tool to derive _Ex-situ priors_ for Bayesian inference in hydrogeology. Ex-situ priors summarize hydrogeological knowledge from studies at similar sites  ([Cucchi et al., 2019](https://www.sciencedirect.com/science/article/abs/pii/S0309170818309059)). The main features of the package are:

- generate prior distributions based on external, ie, ex-situ, data only, therefore meeting the likelihood principle,
- account for possible autocorrelation in these ex-situ data, therefore avoiding the problem of Pseudoreplication,
- account for available soft data, say, in the form of expert information on bounds and moments,
- comes prepackaged with the largest open-source database on hydrogeological parameters (batteries included), etc.

Recent papers have made the case that geostatistics in subsurface hydrology is still falling short on available software tools ([Rubin et al., 2018](https://www.hydrol-earth-syst-sci.net/22/5675/2018/)) and that Bayesian inference is the most relevant framework for subsurface statistics ([Hesse et al., 2019](https://www.frontiersin.org/articles/10.3389/feart.2019.00118/full)). The exPrior package is therefore a timely addition to the R ecosystem of geostatistical tools.

## Installation

Currently, the bset way to install exPrior is through the github repository. Within R, the following commands are to be used

```R
library(devtools)
install_github("GeoStat-Bayesian/exPrior")
```

## Documentation

A publication detailing the algorithm can be found under [Cucchi et al. (2019)](https://www.sciencedirect.com/science/article/abs/pii/S0309170818309059).
