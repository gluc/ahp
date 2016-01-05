context("ahp load")




test_that("Load from string", {
  ahpFile <- system.file("extdata", "car.ahp", package="ahp")
  fileContent <- readChar(ahpFile, file.info(ahpFile)$size)
  carAhp <- LoadString(fileContent)
  Calculate(carAhp)
  df <- Analyze(carAhp)
  expect_equal(ncol(df), 9)
})



test_that("Load vacation", {
  ahpFile <- system.file("extdata", "vacation.ahp", package="ahp")
  vacationAhp <- Load(ahpFile)
  Calculate(vacationAhp)
  df <- Analyze(vacationAhp)
  expect_equal(ncol(df), 5)
})




test_that("Load all examples", {
  for ( ahpFile in list.files(file.path(path.package(package = "ahp"), "extdata")) ) {
    ahpTree <- Load(ahpFile)
    Calculate(ahpTree)
    df <- Analyze(ahpTree)
    expect_true(is.data.frame(df))
    tbl <- AnalyzeTable(ahpTree)
    expect_true(is.formattable(tbl))
    
    params <- expand.grid(
      decisionMaker = c("Total", ahp:::GetDecisionMakers(ahpTree)),
      variable = c("weightContribution", "priority", "score"),
      sort = c("priority", "totalPriority", "orig"),
      stringsAsFactors = FALSE
    )
    
    for (i in nrow(params)) {
      df <- do.call("Analyze", c(ahpTree = ahpTree, as.character(params[i, ])))
      expect_true(is.data.frame(df))
      tbl <- do.call("AnalyzeTable", c(ahpTree = ahpTree, as.character(params[i, ])))
      expect_true(is.formattable(tbl))
    }
    
  }
})
