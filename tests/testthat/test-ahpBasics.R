context("ahp integration")


ahpFile <- system.file("extdata", "car.ahp", package="ahp")
carAhp <- Load(ahpFile)
Calculate(carAhp)
df <- Analyze(carAhp)
tbl <- AnalyzeTable(carAhp)

test_that("ncol", {
  expect_equal(ncol(df), 9)
})


test_that("nrow", {
  expect_equal(nrow(df), 11)
})

test_that("colnames", {
  expect_equal(colnames(df), c(" ", "Weight", "Odyssey", "Accord Sedan", "CR-V", "Element", "Accord Hybrid", "Pilot", "Inconsistency"))
})

test_that("total weight", {
  expect_equal(as.numeric(df[1, 'Weight']), 1)
})


test_that("row sum", {
  rs <- rowSums( df[ , ahp:::GetAlternativesNames(carAhp)] )
  expect_equal(as.numeric(df[, 'Weight']), rs)
})


test_that("alternatives order", {
  aw <-  df[1 , 3:8] 
  expect_equal(aw, sort(aw, decreasing = TRUE))
})


test_that("alternatives order", {
  aw <-  df[1 , 3:8] 
  expect_equal(aw, sort(aw, decreasing = TRUE))
})


test_that("table", {
  expect_true(is.data.frame(tbl))
  expect_true(is.formattable(tbl))
})

test_that("table values", {
  expect_true(all(as.data.frame(tbl)[,2:9] == df[,2:9]))
})


