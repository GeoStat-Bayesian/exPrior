# implement transformation addition to attain normality assumption to MCMC and Bayesian framework
# may need to invest into the previous set up from generalFromMeas
# intial idea: modify carsel function and pass into generalFromMeas

#Remark: flow:
# build noninformative and informative prior distribution
# for non information prior dist, we use jeffrey's rule's suggested by: http://www.stats.org.uk/priors/Priors.pdf
# for informative prior, we use MCMC approach to calculate posterior distribution with chosen
# distribution from its conjugate prior
# since we don't know about the distribution of the data, we will transform everything to normal
# and use its conjugate prior to compute MCMC


## generalFromMeas_transform ##

#'general prior from measurements
#'
#'\code{generalFromMeas} generates general priors from a set of measurements
#'from multiple sites
#'
#'@param meas a dataframe containing measurements to assimilate, with fields val and site_id (see example)
#'@param eval_theta a vector of numerical values of informative prior evaluation points
#'@param niter an integer for the number of samples to use in the MCMC
#'@param hierarchicalSigma a boolean specifying whether the site-specific variance
#'is defined hierarchically by an inverse-gamma distribution (T) or by a prior (F)
#'@param verbose boolean indicating whether R should print information from the progress
#'@return the pdf at values corresponding to theta
#'@examples
#'theta_vect <- seq(from=-10,to=10,by=0.1)
#'df_meas <- data.frame(val=c(c(2,3,4),c(2,1),c(6,7,2,3)),
#'                      site_id=c(rep("a",3),rep("b",2),rep("c",4)))
#'generalFromMeas(meas=df_meas,eval_theta=theta_vect)
#'@export
generalFromMeas_transform = function(meas,
                                     eval_theta,
                                     site_specific_transform = F,
                                     type = c("Log-normal","Box-Cox", "Log-Ratio"),
                                     arg.transform = NULL,
                                     niter=10^5,
                                     hierarchicalSigma=F,
                                     verbose=F){



  #################
  # sanity checks #
  #################

  # check the class of the meas - should be a dataframe
  if(class(meas)!="data.frame"){
    stop(paste0('meas is expected to be a dataframe, ',
                'here it is a ',class(meas),'.\n',
                'Execution halted.'))
  }

  # check that there is a field for numerical values
  if(!("val" %in% names(meas))){
    stop(paste0('Field val is missing from meas.\n',
                'Execution halted.'))
  }

  # check that there is a field for site index
  if(!("site_id" %in% names(meas))){
    stop(paste0('Field site_idx is missing from meas.\n',
                'Execution halted.'))
  }

  method = match.arg(type)
  if (method == "Box-Cox"| method == "Log-Ratio"){
    if (is.null(arg.transform)){
      stop("Transformation argument must be numerical value.")
    }
  }
  if (method == "Log-Ratio" & length(arg.transform) != 2)
    stop("Both lower and upper limit must be provided")


  #######################
  # data transformation #
  #######################

  if (site_specific_transform) {
    if (method == "Log-normal") meas$val = log(meas$val)
    else if (method == "Box-Cox")
      meas$val = forecast::BoxCox(meas$val, arg.transform)
  }else meas$val = gPrior::johnson_sb(meas$val, arg.transform[1], arg.transform[2])






  # may need to check this. According to the paper, Jeffrey's rule is used only when using non-infomative prior
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
        # will need to change this part to: https://r-nimble.org/manuals/NimbleUserManual.pdf (2.2 and 5.2.1)


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

}
