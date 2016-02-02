context("ahp analyze")



test_that("Analyze Identity", {
  ahpFile <- system.file("extdata", "car.ahp", package="ahp")
  carAhp <- Load(ahpFile)
  Calculate(carAhp, pairwiseFun = PrioritiesFromPairwiseMatrixGeometricMean)
  df <- Analyze(carAhp)
  
  expect_equal(ncol(df), 8)
  expect_equal(names(df), c(" ", "Weight", "Odyssey", "Accord Sedan", "CR-V", "Element", "Accord Hybrid", "Pilot"))
  
})


test_that("Analyze Identity 2", {
  ahpFile <- system.file("extdata", "car.ahp", package="ahp")
  carAhp <- Load(ahpFile)
  Calculate(carAhp)
  df <- Analyze(carAhp, variable = "priority")
  
  expect_equal(ncol(df), 9)
  expect_equal(names(df), c(" ", "Priority", "Odyssey", "Accord Sedan", "CR-V", "Element", "Accord Hybrid", "Pilot", "Inconsistency"))
  
})



test_that("Analyze Prune", {
  ahpFile <- system.file("extdata", "car.ahp", package="ahp")
  carAhp <- Load(ahpFile)
  Calculate(carAhp)
  df <- Analyze(
          carAhp,
          sort = "orig",
          pruneFun = function(x, decisionMaker) {
            PruneLevels(x, decisionMaker, 1) &&
              PruneByCutoff(x, decisionMaker, minWeight = 0.05)
          }
        )
  
  expect_equal(nrow(df), 4)
  expect_equal(names(df), c(" ", "Weight", "Accord Sedan", "Accord Hybrid", "Pilot", "CR-V", "Element", "Odyssey", "Inconsistency"))
  
})



test_that("Analyze Prune 2", {
  ahpFile <- system.file("extdata", "car.ahp", package="ahp")
  carAhp <- Load(ahpFile)
  Calculate(carAhp)
  df <- AnalyzeTable(
    carAhp,
    variable = "priority",
    pruneFun = function(x, decisionMaker) {
      PruneByCutoff(x, decisionMaker, minWeight = 0.6)
    }
  )
  
  expect_equal(nrow(df), 1)
  
})





