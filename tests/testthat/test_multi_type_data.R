library(exPrior)
context("output from genExPrior using multiple data types")

test_that("calculations from genExPrior with multiple data types", {
  exdata_S1 = data.frame(val=c(2,4), site_id=rep('S1',2), type=c('bound.min','bound.max'))
  exdata_S2 = data.frame(val=c(2,0.1), site_id=rep('S2',2), type=c('moment.1','moment.2'))
  exdata_S3 = data.frame(val=c(2,3,4), site_id=rep('S3',3), type=c('meas','meas','meas'))
  exdata_multitype <- rbind(exdata_S1, exdata_S2, exdata_S3)
  expect_equal(genExPrior(exdata = exdata_multitype, theta = c(-1,0,1),
                          niter = 10^3, seed = 42)$exPrior$y,
               c(0.1781247, 0.4883154, 0.3550982), tolerance = .05)
})

