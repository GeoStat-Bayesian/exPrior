# Description utils

# --- normalize_pdf ---------
#'
#'Normalize a pdf
#'
#'\code{normalize_pdf} normalizes a pdf so that the integral of the pdf is equal
#'to 1
#'
#'@param x a vector corresponding to values of a random variable X (length strictly greater than 1)
#'@param p_x a vector containing the density of the RV X at locations \code{x}
#'@return The normalized pdf
#'@examples
#'x <- seq(from=-5,to=5,by=0.1)
#'p_x <- 2*dnorm(x)
#'res <- normalize_pdf(x,p_x)
#'plot(x,p_x)
#'lines(x,res$p_x)
#'lines(x,dnorm(x),col='red',lty=2)
#'@export
normalize_pdf <- function(x,p_x){

  # ---- sanity check ----

  if(length(x)!=length(p_x)){
    stop(paste0('In normalize_pdf, x and p_x should have the same length. ',
                 'Here length(x)=',length(x),' and length(p_x)=',length(p_x),'.'))
  }

  if(length(x) <= 1){
    stop(paste0('In normalize_pdf, x and p_x should have a length strictly greater than 1. ',
                 'Here length(x)=',length(x),' and length(p_x)=',length(p_x),'.'))
  }

  # ---- normalize ----
  delta_x <- diff(x)
  mean_px <- (p_x[1:(length(x)-1)] + p_x[2:length(p_x)]) / 2
  norm_constant <- sum(delta_x*mean_px)

  return(list(x=x,
              p_x= (p_x / norm_constant)))

}


# --- smooth_pdf ---------
#'
#'Smooths a pdf
#'
#'\code{smooth_pdf} smmothes a pdf using convolution with a kernel
#'
#'@param x a vector corresponding to values of a random variable X (length strictly greater than 1)
#'@param p_x a vector containing the density of the RV X at locations \code{x}
#'@return The normalized pdf
#'@examples
#'x <- seq(from=-5,to=5,by=0.1)
#'p_x <- dnorm(x) + rnorm(length(x),mean=0,sd=0.02)
#'p_x <- pmax(p_x,0)
#'plot(x,p_x,type='l')
#'res <- smooth_pdf(x,p_x)
#'lines(x,res$p_x,col='red')
smooth_pdf <- function(x,p_x){

  # ---- sanity check ----

  if(length(x)!=length(p_x)){
    stop(paste0('In normalize_pdf, x and p_x should have the same length. ',
                 'Here length(x)=',length(x),' and length(p_x)=',length(p_x),'.'))
  }

  if(length(x) <= 1){
    stop(paste0('In normalize_pdf, x and p_x should have a length strictly greater than 1. ',
                 'Here length(x)=',length(x),' and length(p_x)=',length(p_x),'.'))
  }

  # ---- smooth pdf ----

  res <- numeric(length = length(p_x))

  for(i in 1:length(p_x)){
    res <- res + p_x[i] * dnorm(x,mean = x[i],sd = 2*mean(diff(x)))
  }

  res <- normalize_pdf(x = x,p_x = res)

  return(res)

}
