library(gPrior)
context("output from generalFromMeas")

test_that("calculations from generalFromMeas", {
  expect_equal(generalFromMeas(meas = data.frame(val=c(c(2,3,4),c(2,1),c(6,7,2,3)),
                                                 site_id=c(rep("a",3),rep("b",2),rep("c",4))),
                               eval_theta = c(-1,0,1),niter = 10^3)$gPrior$y,
               c(0.1760710, 0.5032196, 0.3271332),
               tolerance = .05)
})

