context("ahp analyze")


test_that("Analyze Prune", {
  ahpFile <- system.file("extdata", "car.ahp", package="ahp")
  carAhp <- Load(ahpFile)
  Calculate(carAhp)
  df <- Analyze(
          carAhp,
          pruneFun = function(x, decisionMaker) {
            PruneLevels(x, decisionMaker, 1) &&
              PruneByCutoff(x, decisionMaker, minWeight = 0.05)
          }
        )
  
  expect_equal(nrow(df), 4)
  
})



test_that("Analyze Prune", {
  ahpFile <- system.file("extdata", "car.ahp", package="ahp")
  carAhp <- Load(ahpFile)
  Calculate(carAhp)
  df <- AnalyzeTable(
    carAhp,
    pruneFun = function(x, decisionMaker) {
      PruneByCutoff(x, decisionMaker, minWeight = 0.6)
    }
  )
  
  expect_equal(nrow(df), 1)
  
})



