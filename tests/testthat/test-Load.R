context("ahp load")




test_that("Load from string", {
  ahpFile <- system.file("extdata", "car.ahp", package="ahp")
  fileContent <- readChar(ahpFile, file.info(ahpFile)$size)
  carAhp <- LoadString(fileContent)
  Calculate(carAhp)
  df <- GetDataFrame(carAhp)
  expect_equal(ncol(df), 9)
})