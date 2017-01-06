context("ahp visualize")


test_that("Visualize", {
  ahpFile <- system.file("extdata", "car.ahp", package="ahp")
  carAhp <- Load(ahpFile)
  Calculate(carAhp)
  graph <- GetGraph(carAhp)
  expect_equal(class(graph), "dgr_graph")
  expect_equal(dim(graph$nodes_df), c(18, 10))
  expect_equal(names(graph$nodes_df), c("id", "type", "label", "tooltip", "style", "shape", "color", "fillcolor", "penwidth", "fontname"))
  expect_equal(dim(graph$edges_df), c(24, 8))
  expect_equal(names(graph$edges_df), c("id", "from", "to", "rel", "arrowhead", "color", "penwidth", "dir"))
  
})



