# This files contains implentations of the method for computing the prior
# according to Carsel and Parrish

# --- johnson_ln ---------

#'log transform
#'
#'\code{johnson_ln} is the log-transform (1st Johnson transform)
#'
#'@param x vector of original dataset to transform
#'@return the transformed sample
#'@export
johnson_ln <- function(x){
  return(log(x))
}


# --- johnson_sb ---------

#'log ratio transform
#'
#'\code{johnson_sb} is the log-ratio (2nd Johnson transform)
#'
#'@param x vector of original dataset to transform
#'@return the transformed sample
#'@export
johnson_sb <- function(x,a,b){
  return(log((x-a)/(b-x)))
}

# --- johnson_su ---------

#'hyperbolic arcsine transform
#'
#'\code{johnson_su} is the hyperbolic arcsine - transform (3rd Johnson transform)
#'
#'@param x vector of original dataset to transform
#'@return the transformed sample
#'@export
johnson_su <- function(x,a,b){
  u = (x-a)/(b-x)
  return(log(u + sqrt(1+u^2)))
}

# --- goodness_of_fit -----

#' tests goodness of fit to normal distribution
#'
#'\code{goodness_of_fit} tests the closedness to the normal distribution
#'
#'@param y a vector of samples for which to test the normality
#'@return the Kolmogorov-Smirnov statistics
#'@export
goodness_of_fit <- function(y){
  
  y_vals <- pretty(x = range(c(y-diff(range(y)),y+diff(range(y)))),n = 100)
  
  # find first two moments of sample
  mean_y <- mean(y)
  var_y <- var(y) # sample variance
  
  # calculate corresponding normal distribution
  norm_pdf <- dnorm(x = y_vals,mean = mean_y,var_y) # pdf
  
  # calculates normal cdf
  # find mid-points of intervals
  y_mids <- (y_vals[1:(length(y_vals)-1)] + y_vals[2:length(y_vals)]) / 2
  # add first and last point
  y_mids <- c(2*y_vals[1]-y_mids[1],
              y_mids,
              2*y_vals[length(y_vals)]-y_mids[length(y_mids)])
  
  # length of intervals for integration
  diff_y <- diff(y_mids)
  
  # calculate empirical cdf
  norm_cdf <- cumsum(diff_y * norm_pdf)
  
  # calucalte empirical cdf
  ecdf_y <- ecdf(y)(y_vals)
  
  abs_diff <- abs(ecdf_y - norm_cdf)
  
  return(max(abs_diff))
  
}


# --- cap_prior -----

#' calculates the prior according to the Carsel and Parrish methodology
#'
#'\code{cap_prior} calculates the prior according to the Carsel and Parrish methodology
#'
#'@param meas a vector of measurements
#'@param theta values for which to calculate the pdf
#'@return the corresponding pdf
#'@export
cap_prior <- function(meas,
                      theta){
  
  # first get distance for each method
  dist <- rep(NA,length=4)
  names(dist) <- c('no','ln','sb','su')
  dist[1] <- goodness_of_fit(meas)
  if(any(meas<0)){
    dist[2] <- Inf
  }else{dist[2] <- goodness_of_fit(johnson_ln(meas))}
  dist[3] <- goodness_of_fit(johnson_sb(x = meas,a=min(meas)-1,b=max(meas)+1))
  dist[4] <- goodness_of_fit(johnson_su(x = meas,a=min(meas)-1,b=max(meas)+1))
  
  # get name of transform
  johnson_transform <- names(dist)[which(dist==min(dist))]
  
  # calculate the pdf at theta locations
  
  ## generate density in transformed space
  if(johnson_transform=='no'){
    y_vals <- theta
    y <- meas
  }else{
    y_vals <- get(paste0('johnson_',johnson_transform))(x = theta,
                                                        a = min(meas)-1,
                                                        b = max(meas)+1)
    y <- get(paste0('johnson_',johnson_transform))(x = meas,
                                                   a = min(meas)-1,
                                                   b = max(meas)+1)
  }
  f_y <- dnorm(x = y_vals,mean = mean(y),sd = sd(y))
  
  # density in original space is the one calculated 
  # in f_y but in original space (theta locations)
  
  # normalize resulting pdf
  norm_d_x <- generalizedPrior::normalize_pdf(x=theta,p_x=f_y)
  # return result
  return(norm_d_x)
  
}
