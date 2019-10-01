library(exPrior)
context("output from genExPrior")

test_that("calculations from genExPrior", {
  exdata = data.frame(val=c(c(2,3,4),c(2,1),c(6,7,2,3)), site_id=c(rep("a",3),rep("b",2),rep("c",4)))
  theta = c(-1,0,1)
  expect_equal(genExPrior(exdata = exdata, theta = theta, niter = 10^3)$exPrior$y,
               c(0.1834769, 0.4975002, 0.3044085), tolerance = .05)
})

