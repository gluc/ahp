context("ahp integration")



carAhp <- LoadFile("../../inst/extdata/car.ahp")
Calculate(carAhp)
df <- GetDataFrame(carAhp)

test_that("ncol", {
  expect_equal(ncol(df), 9)
})


test_that("nrow", {
  expect_equal(nrow(df), 11)
})

test_that("colnames", {
  expect_equal(colnames(df), c(" ", "Weight", "Odyssey", "Accord Sedan", "CR-V", "Accord Hybrid", "Element", "Pilot", "Consistency"))
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