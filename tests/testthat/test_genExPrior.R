library(exPrior)
context("output from genExPrior")

test_that("calculations from genExPrior", {
  exdata = data.frame(val=c(c(2,3,4),c(2,1),c(6,7,2,3)), site_id=c(rep("a",3),rep("b",2),rep("c",4)))
  expect_equal(genExPrior(exdata = exdata, theta = c(-1,0,1), niter = 10^3, seed = 42)$exPrior$y,
               c(0.1740772, 0.5001419, 0.3342651), tolerance = .01)
})

