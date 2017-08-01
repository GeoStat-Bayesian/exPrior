# This file contains the implementation of
# the Kullback-Leibler divergence and the Kolmogorov-Smirnov distance

# --- KL_divergence ---------

#'Kullback-Leibler divergence
#'
#'\code{KL_divergence} calculates the Kullback-Leibler divergence
#'
#'@param theta a vector of numerics containing values of the RV \eqn{\theta}
#'@param p_theta a vector of numerics containing values of the pdf p at locations theta
#'@param q_theta a vector of numerics containing values of the pdf q at locations theta
#'@return The Kullback-Leibler divergence from q to p
#'@examples
#'theta=seq(from=-5,to=5,by=0.1)
#'p_theta = dnorm(theta,mean = 0.2,sd = 1)
#'q_theta = dnorm(theta,mean = 0.25,sd = 0.5)
#'
# KL_divergence(theta,p_theta,q_theta)
#'@export
KL_divergence <- function(theta,
                          p_theta,
                          q_theta){

  if(length(theta)!=length(p_theta)){
    stop(paste0('theta and p_theta sould have the same length.'))
  }

  if(length(theta)!=length(q_theta)){
    stop(paste0('theta and q_theta sould have the same length.'))
  }

  idx_nonZero = (p_theta!=0) & (q_theta!=0)

  kld <- sum (p_theta[idx_nonZero] * log(p_theta[idx_nonZero] / q_theta[idx_nonZero]) )

  return(kld)

}

# --- KS_distance ---------

#'Kolmogorov-Smirnov distance
#'
#'\code{KS_distance} calculates the Kolmogorov-Smirnov distance betzeen two pdfs.
#'
#'@param theta a vector of numerics containing values of the RV \eqn{\theta}
#'@param p_theta a vector of numerics containing values of the pdf p at locations theta
#'@param q_theta a vector of numerics containing values of the pdf q at locations theta
#'@return The Kolmogorov-Smirnov distance between p and q
#'@examples
#'theta=seq(from=-5,to=5,by=0.1)
#'p_theta = dnorm(theta,mean = 0.2,sd = 1)
#'q_theta = dnorm(theta,mean = 0.25,sd = 0.5)
#'
# KS_distance(theta,p_theta,q_theta)
#'@export
KS_distance <- function(theta,
                        p_theta,
                        q_theta){

  if(length(theta)!=length(p_theta)){
    stop(paste0('theta and p_theta sould have the same length.'))
  }

  if(length(theta)!=length(q_theta)){
    stop(paste0('theta and q_theta sould have the same length.'))
  }

  # find mid-points of intervals
  theta_mids <- (theta[1:(length(theta)-1)] + theta[2:length(theta)]) / 2
  # add first and last point
  theta_mids <- c(2*theta[1]-theta_mids[1],
                  theta_mids,
                  2*theta[length(theta)]-theta_mids[length(theta_mids)])

  # length of intervals for integration
  diff_theta <- diff(theta_mids)

  # integrate with cumulative sum
  # elementwise integration
  p_cdf <- cumsum(diff_theta * p_theta)
  q_cdf <- cumsum(diff_theta * q_theta)

  # calculate maximum of absolute difference
  abs_diff <- abs(p_cdf - q_cdf)

  return(max(abs_diff))

}
