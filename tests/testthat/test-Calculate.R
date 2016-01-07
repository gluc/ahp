context("ahp calculate")


test_that("Calculate Mean", {
  ahpFile <- system.file("extdata", "car.ahp", package="ahp")
  fileContent <- readChar(ahpFile, file.info(ahpFile)$size)
  carAhp <- LoadString(fileContent)
  Calculate(carAhp, pairwiseFun = PrioritiesFromPairwiseMatrixMeanNormalization)
  prio <- carAhp$preferences$DecisionMaker$priority$preferences
  
  expect_equal(sum(prio), 1)
  
})



test_that("Calculate Mean", {
  mat <- matrix( data = c(1, 6, 3, 1/6, 1, 1/2, 1/3, 2, 1), nrow = 3, byrow = TRUE)
  result <- PrioritiesFromPairwiseMatrixMeanNormalization(mat) 
  expect_equal(result$priority, c(2/3, 1/9, 2/9))
})

test_that("Calculate Geometric Mean", {
  mat <- matrix( data = c(1, 6, 3, 1/6, 1, 1/2, 1/3, 2, 1), nrow = 3, byrow = TRUE)
  result <- PrioritiesFromPairwiseMatrixGeometricMean(mat) 
  expect_equal(result$priority, c(2/3, 1/9, 2/9))
})



test_that("Calculate Scores", {
  scores <- c(Rio = 2, Tokyo = 4, Oslo = 3, `Kingston Town` = 0)
  priorities <- PrioritiesFromScoresDefault(scores)
  expect_equal(sum(priorities), 1)
  expect_equal(unname(priorities), c(2/9, 4/9, 3/9, 0))
})
