library(exPrior)
context("output from genExPrior using spatially-correlated data")

test_that("calculations from genExPrior using spatially-correlated data", {
  exdata_spatial <- data.frame("x" = sample(seq(0, 1, 0.01), 22),
                               "y" = sample(seq(0, 1, 0.01), 22),
                               "val" = c(-2.5020, -1.9410, -3.0240, -2.5929, -2.4292, -3.0682,
                                         -2.9953, -2.8178, -2.7236, -1.9657, -2.6567, -2.4977,
                                         -1.1583, -3.0637, -1.6788, -3.5102, -2.3866, -3.4092,
                                         -3.5907, -3.2470, -4.1272, -3.5717),
                               "site_id" = c(rep("S1", 10), rep("S2", 5), rep("S3", 7)))
  expect_equal(genExPrior(exdata = exdata_spatial, theta = c(-1,0,1), seed = 42,
                          spatialCoordinates=TRUE, niter = 10^3)$exPrior$y,
               c(0.4806843, 0.4398900, 0.1322576), tolerance = .05)
})

