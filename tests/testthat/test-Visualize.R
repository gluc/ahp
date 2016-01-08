context("ahp visualize")


test_that("Visualize", {
  ahpFile <- system.file("extdata", "car.ahp", package="ahp")
  carAhp <- Load(ahpFile)
  Calculate(carAhp)
  graph <- GetGraph(carAhp)
  expect_equal(class(graph), "dgr_graph")
  expect_equal(dim(graph$nodes_df), c(18, 8))
  
})



