library(exPrior)
library(gstat)
context("output from genExPrior using spatially-correlated data")

test_that("calculations from genExPrior", {
  set.seed(1)
  xy <- data.frame("x" = sample(seq(0, 1, 0.01), 22),
                   "y" = sample(seq(0, 1, 0.01), 22))
  model = vgm(psill=1, range=1, model='Exp')
  g.dummy <- gstat(formula=z~1, locations=~x+y, dummy=TRUE, beta=1, model=model, nmax=20)
  exdata_spatial <- predict(g.dummy, newdata=xy, nsim=1)

  exdata_spatial[ 1:10, 'sim1'] <- exdata_spatial[ 1:10, 'sim1'] - 3
  exdata_spatial[11:15, 'sim1'] <- exdata_spatial[11:15, 'sim1'] - 2.5
  exdata_spatial[16:22, 'sim1'] <- exdata_spatial[16:22, 'sim1'] - 3.5

  colnames(exdata_spatial)[3] <- "val"
  exdata_spatial$site_id = c(rep("S1", 10), rep("S2", 5), rep("S3", 7))

  theta = c(-1,0,1)

  expect_equal(genExPrior(exdata = exdata_spatial, theta = theta,
                          spatialCoordinates=TRUE, niter = 10^3)$exPrior$y,
               c(0.3518898, 0.4934208, 0.1647697), tolerance = .05)
})

