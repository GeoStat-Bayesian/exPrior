% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pdf_distances.R
\name{KL_divergence}
\alias{KL_divergence}
\title{Kullback-Leibler divergence}
\usage{
KL_divergence(theta, p_theta, q_theta)
}
\arguments{
\item{theta}{a vector of numerics containing values of the RV \eqn{\theta}}

\item{p_theta}{a vector of numerics containing values of the pdf p at locations theta}

\item{q_theta}{a vector of numerics containing values of the pdf q at locations theta}
}
\value{
The Kullback-Leibler divergence from q to p
}
\description{
\code{KL_divergence} calculates the Kullback-Leibler divergence
}
\examples{
theta=seq(from=-5,to=5,by=0.1)
p_theta = dnorm(theta,mean = 0.2,sd = 1)
q_theta = dnorm(theta,mean = 0.25,sd = 0.5)

}
