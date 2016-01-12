context("ahp load")


test_that("Load v1.0", {
  ahpFile <- system.file("extdata", "tom_dick_harry.ahp", package="ahp")
  tdhAhp <- Load(ahpFile)
  Calculate(tdhAhp)
  df <- Analyze(tdhAhp)
  expect_equal(ncol(df), 6)
})



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
  for ( ahpFile in list.files(system.file("extdata", package = "ahp"), recursive = TRUE, include.dirs = FALSE, full.names = TRUE) ) {
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
    
    for (i in 1:nrow(params)) {
      df <- do.call("Analyze", c(ahpTree = ahpTree, as.character(params[i, ])))
      expect_true(is.data.frame(df))
      expect_true(ncol(df) > 3)
      expect_true(nrow(df) >= 4)
      tbl <- do.call("AnalyzeTable", c(ahpTree = ahpTree, as.character(params[i, ])))
      expect_true(is.formattable(tbl))
      expect_true(ncol(tbl) > 3)
      expect_true(nrow(df) >= 4)
      
    }
    
  }
})
