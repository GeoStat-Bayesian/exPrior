---
title: "Illustration on Simple Example"
author: "My Dinh, Karina Cucchi, Falk Heße"
date: "2019-07-06"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Simple Example}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---




When performancing hydrogeological investigation at one site, measurements at that sites $z^*$ are often used to estimate parameter $\theta$. By using the information from the previous studied sites, we wish to derive a regionalized prior distribution for a parameter $\theta$.

This package allows you to compute both regionalized prior distribution and uninformative prior for a parameter $\theta$, and prior and posterior hyperparameter of the Bayesian hierarchical model. 

For simplicity, this documentation only demonstrates the package's application in a three level hierarchical Bayesian model, which is: 

* 1st level: Measurement of $\theta$ from each site. 
* 2nd level: $\theta_j$ is a realization of Gaussian distribution with $\mu_j$ and $\sigma^2_j$, i.e $\theta_j \sim N(\mu_j, \sigma^2_j)$
* 3rd level: $\mu_j$ is a realization of Gaussian distribution with parameter $\alpha$ and $\tau$, defining a common prior pdf for the error variance $\sigma^2$. __$\eta = (\alpha, \tau, \sigma)$__

With this set up, consider an example where the measurements of $\theta$ are available at 3 sites: 

* Site 1: $\theta^*_{1,1} = 2$ 
* Site 2: $\theta^*_{2,1} = 3, \theta^*_{2,2} = 4, \theta^*_{2,3} = 2$ 
* Site 3: $\theta^*_{3,1} = 3, \theta^*_{3,2} = 3, \theta^*_{3,3} = 2$

We want to get the ex-situ prior distribution of $\theta$. We'll solve this question by using function `gen_exPrior`. This function will perform two steps: 

1. Given measurements from similar sites, calculate the posterior distribution for hyperparameters $\eta, f_{\eta}(\eta|\theta^*)$ Markov Chain Monte-Carlo (MCMC) is employed to compute this pdf. 

2. From the udpated hyperparameter distribution, derive the pdf of $\theta$ at new site $S_0$, which is $f_{\Theta}(\theta|\theta^*)$.


Firstly, load the library to get access to this function:


```r
library(exPrior)
library(ggplot2)
library(gtable)
library(grid)
```

Under the assumption that the site specific parameter has normal distribution, the function `gen_exPrior` takes in three parameters. `meas` is a data frame where first column is measurement and second column is a site index where the measurement comes from. `$\theta$` is a vector of numerical values where to evaluate pdf. `niter` is an integer for the sample size in the MCMC that is used to evaluate unknown $\mu_i, \sigma^2_j$ at each site i ( i = 1, 2, 3 in our case). By default it is set to $10^5$, which is an effective sample size for MCMC. Users are free to choose different sample size. 

Putting the measurement of three sites into dataframe, we have: 

```r
meas=data.frame(val = c(c(2) ,c(3,4,2), c(3,3,2)), site_id = c(rep("a",1), rep("b", 3), rep("c",3)))
meas
```

```
##   val site_id
## 1   2       a
## 2   3       b
## 3   4       b
## 4   2       b
## 5   3       c
## 6   3       c
## 7   2       c
```

```r
theta=seq(from=-10,to=10,by=0.1)
```


Run `gen_exPrior` with these arguments, we attained the ex-situ prior distribution and uninformative prior for a parameter $\theta$, and prior and posterior hyperparameter of the Bayesian hierarchical model.


```r
result = gen_exPrior(meas= meas, eval_theta  = theta)
```

```
## defining model...
```

```
## building model...
```

```
## setting data and initial values...
```

```
## running calculate on model (any error reports that follow may simply reflect missing values in model variables) ... 
## checking model sizes and dimensions... This model is not fully initialized. This is not an error. To see which variables are not initialized, use model$initializeInfo(). For more information on model initialization, see help(modelInitialization).
## model building finished.
## compiling... this may take a minute. Use 'showCompilerOutput = TRUE' to see C++ compilation details.
## compilation finished.
```

```
## [1] conjugate_dnorm_dnorm sampler: alpha
## [2] RW sampler: chSqTau
## [3] RW sampler: sigma
## [4] RW sampler: xiTau_negOrPos
## [5] conjugate_dnorm_dnorm sampler: mu[1]
## [6] conjugate_dnorm_dnorm sampler: mu[2]
## [7] conjugate_dnorm_dnorm sampler: mu[3]
## thin = 1: alpha, chSqTau, sigma, xiTau_negOrPos, tau
```

```
## compiling... this may take a minute. Use 'showCompilerOutput = TRUE' to see C++ compilation details.
## compilation finished.
```

```
## |-------------|-------------|-------------|-------------|
## |-------------------------------------------------------|
```

If the distribution of the parameter is not normal, `gen_exPrior` provides an option to transform the distribution to normal under user's choices. Two types of Johnson transformation, logarithm and log ratio, as well as Box-Cox transformation are provided. Lower and upper limit of log ratio, and value of $\lambda$ for Box-Cox transformation should be chosen so that the transformed data has normal distribution. 

We can also visualize the two distributions of $\theta$ using `plot_exPrior` that takes input as output from `gen_exPrior` and Boolean asking whether to plot the measurement.


```r
plot_exPrior(result, plotMeas = T)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

```
## NULL
```


<!-- # From the vignette generated by default -->

<!-- Vignettes are long form documentation commonly included in packages. Because they are part of the distribution of the package, they need to be as compact as possible. The `html_vignette` output type provides a custom style sheet (and tweaks some options) to ensure that the resulting html is as small as possible. The `html_vignette` format: -->

<!-- - Never uses retina figures -->
<!-- - Has a smaller default figure size -->
<!-- - Uses a custom CSS stylesheet instead of the default Twitter Bootstrap style -->

<!-- ## Vignette Info -->

<!-- Note the various macros within the `vignette` section of the metadata block above. These are required in order to instruct R how to build the vignette. Note that you should change the `title` field and the `\VignetteIndexEntry` to match the title of your vignette. -->

<!-- ## Styles -->

<!-- The `html_vignette` template includes a basic CSS theme. To override this theme you can specify your own CSS in the document metadata as follows: -->

<!--     output: -->
<!--       rmarkdown::html_vignette: -->
<!--         css: mystyles.css -->

<!-- ## Figures -->

<!-- The figure sizes have been customised so that you can easily put two images side-by-side. -->

<!-- ```{r, fig.show='hold'} -->
<!-- plot(1:10) -->
<!-- plot(10:1) -->
<!-- ``` -->

<!-- You can enable figure captions by `fig_caption: yes` in YAML: -->

<!--     output: -->
<!--       rmarkdown::html_vignette: -->
<!--         fig_caption: yes -->

<!-- Then you can use the chunk option `fig.cap = "Your figure caption."` in **knitr**. -->

<!-- ## More Examples -->

<!-- You can write math expressions, e.g. $Y = X\beta + \epsilon$, footnotes^[A footnote here.], and tables, e.g. using `knitr::kable()`. -->

<!-- ```{r, echo=FALSE, results='asis'} -->
<!-- knitr::kable(head(mtcars, 10)) -->
<!-- ``` -->

<!-- Also a quote using `>`: -->

<!-- > "He who gives up [code] safety for [code] speed deserves neither." -->
<!-- ([via](https://twitter.com/hadleywickham/status/504368538874703872)) -->
