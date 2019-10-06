  ## genExPrior ##

#'ex-situ prior from ex-situ data
#'
#'\code{genExPrior} generates ex-situ priors from a set of ex-situ data
#'from multiple sites
#'
#'@param exdata a dataframe containing ex-situ data to assimilate, with fields val
#'  and site_id (see example)
#'@param theta a vector of numerical values of informative prior evaluation
#'  points
#'@param niter (optional) an integer for the number of samples to use in the
#'  MCMC
#'@param range_alpha (optional) a vector of two values corresponding to the
#'  lower and the upper bounds of the uniform distribution for alpha
#'@param hierarchicalSigma (optional) a boolean specifying whether the
#'  site-specific variance is defined hierarchically by an inverse-gamma
#'  distribution (T) or by a prior (F)
#'@param spatialCoordinates (optional) a boolean specifying whether spatial
#'  coordinates are provided as covariates to numerical ex-situ data. If T, the
#'  spatial autocorrelation of ex-situ data is accounted for, assuming that the
#'  spatial covariance has an exponential form.
#'@param verbose (optional) boolean indicating whether R should print
#'  information from the progress
#'@return the pdf at values corresponding to theta
#'@examples
#'theta <- seq(from=-5,to=5,by=1)
#'exdata <- data.frame(val=c(c(2,3,4),c(2,1),c(6,7,2,3)),
#'                   site_id=c(rep("a",3),rep("b",2),rep("c",4)),
#'                   x = c(c(2,3,4),c(2,3),c(2,2,3,3)),
#'                   y = c(c(2,2,3),c(3,2),c(2,3,2,3)))
#' genExPrior(exdata=exdata,theta=theta)
#'@export
genExPrior <- function(exdata,
                       theta,
                       niter=10^5,
                       range_alpha=NULL,
                       hierarchicalSigma=F,
                       spatialCoordinates=F,
                       verbose=F){

  #################
  # sanity checks #
  #################

  # check the class of the exdata - should be a dataframe
  if(class(exdata)!="data.frame"){
    stop(paste0('exdata is expected to be a dataframe, ',
                'here it is a ',class(exdata),'.\n',
                'Execution halted.'))
  }

  # check that there is a field for numerical values
  if(!("val" %in% names(exdata))){
    stop(paste0('Field val is missing from exdata.\n',
                'Execution halted.'))
  }

  # check that there is a field for site index
  if(!("site_id" %in% names(exdata))){
    stop(paste0('Field site_id is missing from exdata.\n',
                'Execution halted.'))
  }

  #########################################
  # define range for hyperparameter alpha #
  #########################################

  if(is.null(range_alpha)){
    range_alpha <- range(theta)
  }

  #######################
  # Define NIMBLE model #
  #######################

  # transform dataframe to list of ex-situ data
  list_exdata <- plyr::dlply(exdata, .(site_id))
  # list_exdata is a list of vectors
  # containing ex-situ data at each site

  # --
  # first useful constants
  # --

  I = length(list_exdata)                    # number of sites
  if(verbose){cat(I,'sites detected.')}
  J_i = as.numeric(unlist(lapply(list_exdata,nrow)))   # number of measurement per site
  nbexdataMax <- max(J_i)                    # maximum number of ex-situ data at one site
  if(verbose){cat(paste0('\n... number of ex-situ data per sites varying between ',
                         min(J_i),' and ',max(J_i),'.'))}

  # --
  # define data (observations)
  # --

  # construct matrix of ex-situ data
  # rows are sites
  # columns are ex-situ data at site defined by row
  exdataMatrix <- array(data = NA,dim = c(I,nbexdataMax))
  for(iList in 1:length(list_exdata)){
    exdataMatrix[iList,1:nrow(list_exdata[[iList]])] <- list_exdata[[iList]]$val
  }

  siteData <- list(theta = exdataMatrix)

  # --
  # define constants
  # --

  if(!spatialCoordinates){ # if independent ex-situ data within one site

    siteConst <- list(I = I,
                      J_i=as.numeric(J_i))

  }else{ # if spatial autocorrelation

    # construct matrix containing distance between ex-situ data within each site
    matrix_dist <- array(NA, dim = c(nbexdataMax,nbexdataMax,I))
    for(i in 1:I){
      matrix_dist[1:J_i[i],1:J_i[i],i] <-
        as.matrix(dist(cbind(x=list_exdata[[i]]$x,
                             y=list_exdata[[i]]$y)))
    }

    siteConst <- list(I = I,
                      J_i=as.numeric(J_i),
                      matrix_dist = matrix_dist,
                      matrix_ones = matrix(data = 1, # for trick for defining one mean per site
                                           nrow=I,ncol=nbexdataMax))

  }


  # --
  # define hierarchical model using BUGS code
  # --

  if(hierarchicalSigma){ # if sigma is defined using a hierarchical model

    siteHierarchyCode <-

      nimble::nimbleCode({

        # prior distribution of hyperparameters
        alpha ~ dunif(min = range_alpha[1],max = range_alpha[2])
        tau ~ dunif(min = 0, max = 2)
        beta ~ dunif(min = 0, max = 5)
        xi ~ dunif(min = 0, max = 2)

        # hierarchical model at each site
        for (i in 1:I){ # loop over sites
          mu[i] ~ dnorm(mean = alpha,sd = tau) # distribution of mean at site i
          inv_sigma2[i] ~ dgamma(shape = beta, rate = xi)
          sigma2[i] <- 1/inv_sigma2[i] # distribution of sigma2 at site i
          # distribution of ex-situ data conditional on mu[i] and sigma2[i]
          for (j in 1:J_i[i]){ # loop over ex-situ data
            theta[i,j] ~ dnorm(mean = mu[i],sd = sqrt(sigma2[i]))
          }
        }

      })

  }else{

    if(!spatialCoordinates){ # site-specific independence of ex-situ data

      siteHierarchyCode <-

        nimble::nimbleCode({

          # prior distribution of hyperparameters
          # see http://www.stats.org.uk/priors/Priors.pdf for formulation of approximations
          # approximates flat prior
          alpha ~ dnorm(mean = 0,sd = 1000)
          # constructs half-Cauchy distribution
          # see http://journals.plos.org/plosone/article/file?type=supplementary&id=info:doi/10.1371/journal.pone.0029215.s003
          prec <- 1/(25^2) # precision when scale = 25
          xiTau_negOrPos ~ dnorm(0, prec)
          xiTau <- abs(xiTau_negOrPos)
          chSqTau ~ dgamma(0.5,0.5)
          tau <- xiTau/sqrt(chSqTau)
          # approximates Jeffrey's prior (inverse prior)
          sigma ~ dgamma(shape = 0.0001, rate = 0.0001)

          # hierarchical model at each site
          for (i in 1:I){ # loop over sites
            mu[i] ~ dnorm(mean = alpha,sd = tau) # distribution of mean at site i
            # distribution of ex-situ data conditional on mu[i] and sigma2[i]
            for (j in 1:J_i[i]){ # loop over ex-situ data
              theta[i,j] ~ dnorm(mean = mu[i],sd = sigma)
            }
          }

        })


    }else{ # spatial correlation case

      siteHierarchyCode <-

        nimble::nimbleCode({

          # prior distribution of hyperparameters
          # see http://www.stats.org.uk/priors/Priors.pdf for formulation of approximations
          # approximates flat prior
          alpha ~ dnorm(mean = 0,sd = 1000)
          # constructs half-Cauchy distribution
          # see http://journals.plos.org/plosone/article/file?type=supplementary&id=info:doi/10.1371/journal.pone.0029215.s003
          prec <- 1/(25^2) # precision when scale = 25
          xiTau_negOrPos ~ dnorm(0, prec)
          xiTau <- abs(xiTau_negOrPos)
          chSqTau ~ dgamma(0.5,0.5)
          tau <- xiTau/sqrt(chSqTau)
          # approximates Jeffrey's prior (inverse prior)
          sigma ~ dgamma(shape = 0.0001, rate = 0.0001)
          # approximates Jeffrey's prior (inverse prior)
          lambda ~ dgamma(shape = 0.0001, rate = 0.0001)

          # hierarchical model at each site
          for (i in 1:I){ # loop over sites

            mu[i] ~ dnorm(mean = alpha,sd = tau) # distribution of mean at site i
            mat_mu_i[i,1:J_i[i]] <- mu[i] * matrix_ones[i,1:J_i[i]]

            # the covariance matrix is defined by an exponential covariance function
            Sigma[1:J_i[i],1:J_i[i],i] <-
              sigma^2 * (exp( -(matrix_dist[1:J_i[i],
                                            1:J_i[i],
                                            i]/lambda) ))

            # observations are multivariate normal
            theta[i,1:J_i[i]] ~ dmnorm(mat_mu_i[i,1:J_i[i]],
                                       cov = Sigma[1:J_i[i],1:J_i[i],i])

          }

        })

    }

  }

  # --
  # define initial values
  # --

  if(hierarchicalSigma){
    siteInit <- list(alpha = 0,
                     tau = 1,
                     beta = 2,
                     xi = 1,
                     # list containing ex-situ data vectors only
                     mu = unlist(lapply(X = lapply(list_exdata, '[[', 'val'),
                                        FUN = mean,na.rm=T)),
                     sigma2 = rep(1,length(list_exdata)))
  }else{
    if(!spatialCoordinates){
      siteInit <- list(alpha = 0, # mean of means
                       sigma = 1, # sd
                       tau = 1, # sd of means
                       # list containing ex-situ data vectors only
                       mu = unlist(lapply(X = lapply(list_exdata, '[[', 'val'),
                                          FUN = mean,na.rm=T)))
    }else{

      # initialize Sigma with unit matrices
      Sigma_0 <- array(NA, dim = c(nbexdataMax,nbexdataMax,I))
      for(i in 1:I){
        Sigma_0[1:J_i[i],1:J_i[i],i] <- diag(J_i[i]) # J_i[i] ex-situ data at site i
      }

      siteInit <- list(alpha = 0, # mean of means
                       sigma = 1, # sd
                       tau = 1, # sd of means
                       lambda = 1, # integral scale of spatial autocorrelation
                       # list containing ex-situ data vectors only
                       mu = unlist(lapply(X = lapply(list_exdata, '[[', 'val'),
                                          FUN = mean,na.rm=T)),
                       mat_mu_i = matrix(data = 1,
                                         nrow = I,ncol = nbexdataMax),
                       Sigma = Sigma_0) # unit matrices (independent ex-situ data)
    }
  }

  # actually create the model
  siteModel <-
    nimble::nimbleModel(code = siteHierarchyCode,
                        constants = siteConst,
                        data = siteData,
                        inits = siteInit,
                        name = 'siteModel')

  # check that the model makes sense
  # siteModel$getNodeNames()

  # compile the model
  CsiteModel <- nimble::compileNimble(siteModel)

  ##########################
  # Configure and run MCMC #
  ##########################

  # names of hyperparameters to monitor
  if(hierarchicalSigma){
    hyperPar = c('alpha', 'tau', 'beta', 'xi')
  }else{
    if(!spatialCoordinates){
      hyperPar = c('alpha', 'tau', 'sigma')
    }else{
      hyperPar = c('alpha', 'tau', 'sigma', 'lambda')
    }

  }


  # create compile and run MCMC
  siteModelConf <- nimble::configureMCMC(model = siteModel,print=T)

  siteModelConf$addMonitors(hyperPar)

  siteModelMCMC <- nimble::buildMCMC(siteModelConf)
  CsiteModelMCMC <- nimble::compileNimble(siteModelMCMC, project = siteModel)

  CsiteModelMCMC$run(niter)
  MCMCsamples <- as.matrix(CsiteModelMCMC$mvSamples)

  # effectiveSize function from library coda
  MCMC_effectiveSizes = numeric(length(hyperPar))
  for(i in 1:length(MCMC_effectiveSizes)){
    MCMC_effectiveSizes[i] = coda::effectiveSize(MCMCsamples[ , hyperPar[i] ])
  }

  ###############################################################
  # Calculate prior and posterior densities for hyperparameters #
  ###############################################################

  # names of hyperparameters are defined in vector hyperPar

  ## sample from prior
  d_hyperPar_prior = list()


  if(hierarchicalSigma){

    # define hyperprior for alpha
    d_hyperPar_prior[[1]] <-
      data.frame(x=seq(from=range_alpha[1],to=range_alpha[2],length.out = 100))
    d_hyperPar_prior[[1]]$y <-
      dunif(d_hyperPar_prior[[1]]$x,min = range_alpha[1],max = range_alpha[2])

    # define hyperprior for tau
    x_tau <- seq(from=0.001,to=2,by=0.001)
    y_tau <- dunif(x_tau,min = 0,max = 2)
    d_hyperPar_prior[[2]] <- data.frame(x=x_tau,y=y_tau)
    # for Jeffrey's prior case
    # y_tau <- dgamma(x = x_tau,shape = 0.001,rate = 0.001)
    # pdf_tau_norm <- normalize_pdf(x = x_tau,p_x = y_tau)
    # d_hyperPar_prior[[2]] <- data.frame(x=pdf_tau_norm$x,y=pdf_tau_norm$p_x)

    # define hyperprior for beta
    d_hyperPar_prior[[3]] <- data.frame(x=seq(from=-0.5,to=5.5,by=0.01))
    d_hyperPar_prior[[3]]$y <- dunif(d_hyperPar_prior[[3]]$x,min = 0, max = 5)

    # define hyperprior for xi
    d_hyperPar_prior[[4]] <- data.frame(x=seq(from=-0.5,to=2.5,by=0.01))
    d_hyperPar_prior[[4]]$y <- dunif(d_hyperPar_prior[[4]]$x,min = 0, max = 2)

  }else{

    # define hyperprior for alpha
    d_hyperPar_prior[['alpha']] <- data.frame(x = seq(from = range_alpha[1],
                                                      to = range_alpha[2],
                                                      length.out = 100))
    # define boundaries for the hyperprior
    d_hyperPar_prior[['alpha']]$y <- dnorm(x = d_hyperPar_prior[['alpha']]$x,
                                           mean = 0,sd = 1000)

    # define hyperprior for tau
    d_hyperPar_prior[['tau']] <- data.frame(x = seq(from = 0.001, to = 2,
                                                    by = 0.001))
    # define boundaries for the hyperprior
    d_hyperPar_prior[['tau']]$y <- stats::dcauchy(x = d_hyperPar_prior[['tau']]$x,
                                                  location = 0,scale = 25)

    # define hyperprior for sigma
    d_hyperPar_prior[['sigma']] <- data.frame(x = seq(from = 0.001, to = 2,
                                                      by = 0.001))
    # define boundaries for the hyperprior
    d_hyperPar_prior[['sigma']]$y <- dgamma(x = d_hyperPar_prior[['sigma']]$x,
                                            shape = 0.0001, rate = 0.0001)

    # define hyperprior for lambda
    if(spatialCoordinates){
      d_hyperPar_prior[['lambda']] <- data.frame(x = seq(from = 0.001, to = 2,
                                                         by = 0.001))
      # define boundaries for the hyperprior
      d_hyperPar_prior[['lambda']]$y <- dgamma(x = d_hyperPar_prior[['lambda']]$x,
                                               shape = 0.0001, rate = 0.0001)
    }


    # now define also hyperprior for mu
    # need to sample from N(alpha,tau)
    samples_alpha <- rnorm(n = niter,mean = 0,sd = 1000)
    samples_tau <- rcauchy(n = niter,location = 0,scale = 25)
    samples_mu <- rnorm(n = niter,mean = samples_alpha,sd = abs(samples_tau))
    d_mu <- density(samples_mu,from = min(theta),to=max(theta))
    d_mu <- exPrior::normalize_pdf(d_mu$x,d_mu$y)
    d_hyperPar_prior[['mu']] <- data.frame(x=theta)
    d_hyperPar_prior[['mu']]$y <- approx(x = d_mu$x,
                                         y = d_mu$p_x,
                                         xout = d_hyperPar_prior[['mu']]$x,
                                         yleft=0,yright=0)$y
  }

  ## sample from posterior

  d_hyperPar_post = list()
  for(i in 1:length(hyperPar)){
    # calculate density from MCMC samples using kernels
    # from and to define the bounds to have a good resolution between bounds of interest
    d_post <- density(MCMCsamples[,hyperPar[i]],
                      from = min(pretty(d_hyperPar_prior[[i]]$x)),
                      to = max(pretty(d_hyperPar_prior[[i]]$x)))
    # interpolate density at points corresponding to hyperpriors
    d_hyperPar_post[[i]] <- approx(x = d_post$x,
                                   y = d_post$y,
                                   xout = d_hyperPar_prior[[i]]$x,
                                   yleft=0,yright=0) # define probability 0 outside density bounds
  }

  #######################################################
  # Sample theta from posterior predictive distribution #
  #######################################################

  # We can sample from the posterior predictive distribution by sampling from
  # each eta sample from the Monte-Carlo Markov Chain.

  # draw one theta sample per eta value
  samp_theta_pred = numeric(length = 100*nrow(MCMCsamples))
  # loop over samples in MCMC
  for(iEta in 1:nrow(MCMCsamples)){

    if(hierarchicalSigma){

      # draw realizations of the site-specific parameters
      mu_s = rnorm(n = 10,
                   mean = MCMCsamples[iEta,'alpha'],
                   sd = MCMCsamples[iEta,'tau'])
      inv_sigma2_s = rgamma(n = 10,
                            shape = MCMCsamples[iEta,'beta'],
                            rate = MCMCsamples[iEta,'xi'])
      sigma2_s <- 1/inv_sigma2_s

      # draw realizations of the physical parameter
      for(iPhi in 1:10){
        samp_theta_pred[100*(iEta-1)+10*(iPhi-1)+(1:10)] <-
          rnorm(n = 10,
                mean = mu_s[iPhi],
                sd = sqrt(sigma2_s[iPhi]))
      }

    }else{

      # draw realizations of the mean at the new site
      mu_s <- rnorm(n = 10,
                    mean = MCMCsamples[iEta,'alpha'],
                    sd = MCMCsamples[iEta,'tau'])

      # draw realizations of the physical parameter
      for(iPhi in 1:10){
        samp_theta_pred[100*(iEta-1)+10*(iPhi-1)+(1:10)] <-
          rnorm(n = 10,
                mean = mu_s[iPhi],
                sd = MCMCsamples[iEta,'sigma'])
      }

    }

  }

  # limit estimation to region specified by bounds on theta (in theta)
  idx <- which(samp_theta_pred <= min(theta) | samp_theta_pred >= max(theta))
  samp_theta_pred[idx] <- NA

  # calculate non parametric density
  density_theta <- density(samp_theta_pred,na.rm = T)

  # interpolate density on desired points theta given in argument
  d_theta_pred = as.data.frame(approx(x = density_theta$x,
                                      y = density_theta$y,
                                      xout = theta,yleft=0,yright=0))

  ###################################################
  # Calculate uninformative distribution for theta  #
  ###################################################

  # For comparison with informative case

  samp_theta_prior = numeric(length = 1000*nrow(MCMCsamples))

  # here multiplied by 50 because it is harder to reach convergence
  for(iEta in 1:(10*nrow(MCMCsamples))){

    if(hierarchicalSigma){

      #sample eta from prior distribution
      alpha <- runif(1,min = range_alpha[1],max = range_alpha[2])
      tau <- runif(1,min = 0, max = 2)
      beta <- runif(1,min = 0, max = 5)
      xi <- runif(1,min = 0, max = 2)

      # sample site-specific parameters
      mu_s = rnorm(n = 10,
                   mean = alpha,
                   sd = tau)
      inv_sigma2_s = rgamma(n = 10,
                            shape = beta,
                            rate = xi)
      sigma2_s <- 1/inv_sigma2_s

      # sample theta parameters
      for(iPhi in 1:10){
        samp_theta_prior[100*(iEta-1)+10*(iPhi-1)+(1:10)] <-
          rnorm(n = 10,
                mean = mu_s[iPhi],
                sd = sqrt(sigma2_s[iPhi]))
      }

    }else{

      # sample eta from prior distribution
      alpha <- rnorm(n = 1, mean = 0,sd = 1000) # approximates flat prior
      tau <- rcauchy(n = 1, location = 0,scale = 25) # half-Cauchy prior
      if(tau < 0 ) {tau <- (-tau)}

      # sample site-specific parameters
      mu_s <- rnorm(n = 10, mean = alpha, sd = tau)
      sigma <- rgamma(n = 10, shape = 0.0001, rate = 0.0001) # approximates Jeffrey's prior (inverse prior)

      # sample theta parameters
      for(iPhi in 1:10){
        samp_theta_prior[100*(iEta-1)+10*(iPhi-1)+(1:10)] <-
          rnorm(n = 10,
                mean = mu_s[iPhi],
                sd = sigma[iPhi])
      }

    }

  }

  # limit estimation to region specified by bounds on theta
  idx <- which(samp_theta_prior <= min(theta) | samp_theta_prior >= max(theta))
  samp_theta_prior[idx] <- NA

  # calculate non parametric density
  density_theta <- density(samp_theta_prior,na.rm = T)

  # interpolate density on desired points theta given in argument
  d_theta_prior = as.data.frame(stats::approx(x = density_theta$x,
                                              y = density_theta$y,
                                              xout = theta,
                                              yleft=0,yright=0))

  ####################
  ## return results ##
  ####################

  return(list(exPrior=d_theta_pred, # regionalized prior for theta
              uPrior=d_theta_prior, # uninformative prior for theta
              hyperPar=hyperPar, # list of hyperparameters
              d_hyperPar=list(d_hyperPar_prior=d_hyperPar_prior, # prior for hyperparameters
                              d_hyperPar_post=d_hyperPar_post), # posterior for hyperparameters
              exdata=exdata, # ex-situ data as given in function arguments
              MCMC=list(MCMCsamples=MCMCsamples,
                        MCMC_effectiveSizes=MCMC_effectiveSizes)))

}
