context("ahp load")




test_that("Load from string", {
  ahpFile <- system.file("extdata", "car.ahp", package="ahp")
  fileContent <- readChar(ahpFile, file.info(ahpFile)$size)
  carAhp <- LoadString(fileContent)
  Calculate(carAhp)
  df <- GetDataFrame(carAhp)
  expect_equal(ncol(df), 9)
})



test_that("Load vacation", {
  ahpFile <- system.file("extdata", "vacation.ahp", package="ahp")
  vacationAhp <- LoadFile(ahpFile)
  Calculate(vacationAhp)
  df <- GetDataFrame(vacationAhp)
  expect_equal(ncol(df), 5)
})