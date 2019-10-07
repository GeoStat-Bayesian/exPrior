library(exPrior)
context("output from genExPrior using spatially-correlated data")

test_that("calculations from genExPrior", {
  set.seed(1)
  exdata_spatial <- data.frame("x" = sample(seq(0, 1, 0.01), 22),
                               "y" = sample(seq(0, 1, 0.01), 22),
                               "val" = c(0.498030960,  1.059053432, -0.024007514,
                                         0.407121809,  0.570833374, -0.068272452,
                                         0.004719800,  0.182258222,  0.276415717,
                                         1.034386868, -0.156727395,  0.002374838,
                                         1.341720288, -0.563782051,  0.821210602,
                                        -0.010202173,  1.113467721,  0.090853882,
                                        -0.090706486 , 0.253096456, -0.627293638,
                                        -0.071735270))

  exdata_spatial[ 1:10, 'val'] <- exdata_spatial[ 1:10, 'val'] - 3
  exdata_spatial[11:15, 'val'] <- exdata_spatial[11:15, 'val'] - 2.5
  exdata_spatial[16:22, 'val'] <- exdata_spatial[16:22, 'val'] - 3.5

  exdata_spatial$site_id = c(rep("S1", 10), rep("S2", 5), rep("S3", 7))

  theta = c(-1,0,1)

  expect_equal(genExPrior(exdata = exdata_spatial, theta = theta,
                          spatialCoordinates=TRUE, niter = 10^3)$exPrior$y,
               c(0.4296820, 0.4418741, 0.1310425), tolerance = .05)
})

