
## generalFromMeas ##

#'general prior from measurements
#'
#'\code{generalFromMeas} generates general priors from a set of measurements
#'from multiple sites
#'
#'@param meas a list of vector of numerical values containing the measurement
#'  dataset to assimilate
#'@param theta a vector of numerical values where to evaluate the pdf
#'@param niter an integer for the number of samples to use in the MCMC
#'@return the pdf at values corresponding to theta
#'@examples
#'theta=seq(from=-10,to=10,by=0.1)
#'meas=list(c(2,3,4),c(2,1),c(6,7,2,3))
#'generalFromMeas(meas,theta)
#'@export
generalFromMeas <- function(meas,
                            theta,
                            niter=10^5,
                            hierarchicalSigma=F){
  
  #################################
  # define data from measurements #
  #################################
  
  # meas is a list of measurements
  # transform to matrix
  I = length(meas)                       # number of sites
  J_i = unlist(lapply(meas,length))      # number of measurement per site
  nbMeasMax <- max(J_i)                  # maximum number of measurements at one site
  
  # create empty matrix with all NA
  measMatrix <- array(data = NA,dim = c(I,nbMeasMax))
  
  # fill matrix with values
  for(iList in 1:length(meas)){
    measMatrix[iList,1:length(meas[[iList]])] <- meas[[iList]]
  }
  
  siteData <- list(theta = measMatrix)
  
  #############################################
  # define hierarchical model using bugs code #
  #############################################
  
  if(hierarchicalSigma){ # if sigma is defined using a hierarchical model
    
    siteHierarchyCode <-
      
      nimble::nimbleCode({
        
        # prior distribution of hyperparameters
        alpha ~ dunif(min = -10,max = 10)
        tau ~ dunif(min = 0, max = 2)
        beta ~ dunif(min = 0, max = 5)
        xi ~ dunif(min = 0, max = 2)
        
        # hierarchical model at each site
        for (i in 1:I){ # loop over sites
          mu[i] ~ dnorm(mean = alpha,sd = tau) # distribution of mean at site i
          inv_sigma2[i] ~ dgamma(shape = beta, rate = xi)
          sigma2[i] <- 1/inv_sigma2[i] # distribution of sigma2 at site i
          # distribution of measurements conditional on mu[i] and sigma2[i]
          for (j in 1:J[i]){ # loop over measurements
            theta[i,j] ~ dnorm(mean = mu[i],sd = sqrt(sigma2[i]))
          }
        }
        
      })
    
  }else{
    
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
          # distribution of measurements conditional on mu[i] and sigma2[i]
          for (j in 1:J[i]){ # loop over measurements
            theta[i,j] ~ dnorm(mean = mu[i],sd = sigma)
          }
        }
        
      })
    
  }
  
  siteConst <- list(I = nrow(measMatrix),J=J_i)
  
  if(hierarchicalSigma){
    siteInit <- list(alpha = 0,
                     tau = 1,
                     beta = 2,
                     xi = 1,
                     mu = unlist(lapply(X = meas,FUN = mean,na.rm=T)),
                     sigma2 = rep(1,length(meas)))
  }else{
    siteInit <- list(alpha = 0, # mean of means
                     sigma = 1, # sd
                     tau = 1, # sd of means
                     mu = unlist(lapply(X = meas,FUN = mean,na.rm=T)))
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
    which_monitor = c('alpha', 'tau', 'beta', 'xi')
  }else{
    which_monitor = c('alpha', 'tau', 'sigma')
  }
  
  
  # create compile and run MCMC
  siteModelConf <- nimble::configureMCMC(model = siteModel,print=T)
  
  siteModelConf$addMonitors(which_monitor)
  
  siteModelMCMC <- nimble::buildMCMC(siteModelConf)
  CsiteModelMCMC <- nimble::compileNimble(siteModelMCMC, project = siteModel)
  
  CsiteModelMCMC$run(niter)
  MCMCsamples <- as.matrix(CsiteModelMCMC$mvSamples)
  
  # effectiveSize function from library coda
  MCMC_effectiveSizes = numeric(length(which_monitor))
  for(i in 1:length(MCMC_effectiveSizes)){
    MCMC_effectiveSizes[i] = effectiveSize(MCMCsamples[ , which_monitor[i] ])
  }
  
  ###############################################################
  # Calculate prior and posterior densities for hyperparameters #
  ###############################################################
  
  hyperPar = which_monitor
  
  ## sample from prior
  d_hyperPar_prior = list()
  
  
  if(hierarchicalSigma){
    
    # define hyperprior for alpha
    d_hyperPar_prior[[1]] <- data.frame(x=seq(from=-10,to=10,by=0.1))
    d_hyperPar_prior[[1]]$y <- dunif(d_hyperPar_prior[[1]]$x,min = -10,max = 10)
    
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
    d_hyperPar_prior[[1]] <- data.frame(x=seq(from=-10,to=10,by=0.1)) # define boundaries for the hyperprior
    d_hyperPar_prior[[1]]$y <- dnorm(x = d_hyperPar_prior[[1]]$x,mean = 0,sd = 1000)
    
    # define hyperprior for tau
    d_hyperPar_prior[[2]] <- data.frame(x=seq(from=0.001,to=2,by=0.001)) # define boundaries for the hyperprior
    d_hyperPar_prior[[2]]$y <- dcauchy(x = d_hyperPar_prior[[2]]$x, location = 0,scale = 25)
    
    # define hyperprior for sigma
    d_hyperPar_prior[[3]] <- data.frame(x=seq(from=0.001,to=2,by=0.001)) # define boundaries for the hyperprior
    d_hyperPar_prior[[3]]$y <- dgamma(x = d_hyperPar_prior[[3]]$x, shape = 0.0001, rate = 0.0001)
    
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
  
  # limit estimation to region specified by bounds on theta
  idx <- which(samp_theta_pred <= min(theta) | samp_theta_pred >= max(theta))
  samp_theta_pred[idx] <- NA
  
  # calculate non parametric density
  density_theta <- density(samp_theta_pred,na.rm = T)
  
  # interpolate density on desired points theta given in argument
  d_theta_pred = approx(x = density_theta$x,
                        y = density_theta$y,
                        xout = theta,yleft=0,yright=0)
  
  ###################################################
  # Calculate uninformative distribution for theta  #
  ###################################################
  
  # For comparison with informative case
  
  samp_theta_prior = numeric(length = 1000*nrow(MCMCsamples))
  
  # here multiplied by 10 because it is harder to reach convergence
  for(iEta in 1:(10*nrow(MCMCsamples))){ 
    
    if(hierarchicalSigma){
      
      #sample eta from prior distribution
      alpha <- runif(1,min = -10,max = 10)
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
  d_theta_prior = approx(x = density_theta$x,
                         y = density_theta$y,
                         xout = theta,yleft=0,yright=0)
  
  ####################
  ## return results ##
  ####################
  
  return(list(gPrior=d_theta_pred, # regionalized prior for theta
              uPrior=d_theta_prior, # uninformative prior for theta
              hyperPar=hyperPar, # list of hyperparameters
              d_hyperPar=list(d_hyperPar_prior=d_hyperPar_prior, # prior for hyperparameters
                              d_hyperPar_post=d_hyperPar_post), # posterior for hyperparameters
              meas=meas,
              MCMC=list(MCMCsamples=MCMCsamples,
                        MCMC_effectiveSizes=MCMC_effectiveSizes)))
  
}

