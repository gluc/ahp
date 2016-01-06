context("ahp analyze")


test_that("Analyze Prune", {
  ahpFile <- system.file("extdata", "car.ahp", package="ahp")
  carAhp <- Load(ahpFile)
  Calculate(carAhp)
  df <- Analyze(carAhp,
                pruneFun = function(x, decisionMaker) {
                  PruneLevels(x, decisionMaker, 1) &&
                    PruneByCutoff(x, decisionMaker, minWeight = 0.05)
                  }
                )
  
  expect_equal(dim(df), c(4, 9))
  expect_equal(round(as.numeric(df$Weight), 6), c(1, 0.510075, 0.234352, 0.215054))
  
})


