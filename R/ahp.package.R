#' ahp AHP (Analytic Hierarchy Process) Modeling for R
#' 
#' AHP (Analytic Hierarchy Process) is a decision making framework developed by Thomas Saaty.
#' This package lets you model and analyse complex decision making problems according to the AHP framework.  
#' 
#' The basic workflow with this package is:
#' 1. specify your ahp problem in an ahp file
#' 2. load ahp file, using \code{\link{LoadFile}}
#' 3. calculate model, using \code{\link{Calculate}}
#' 4. output model analysis, either using \code{\link{GetDataFrame}} or using \code{\link{ShowTable}}
#' 
#' For more information, see the package vignette using \code{vignette("car-example", package = "ahp")}
#' 
#' @examples
#' library(ahp)
#' ahpFile <- system.file("extdata", "car.ahp", package="ahp")
#' carAhp <- LoadFile(ahpFile)
#' Calculate(carAhp)
#' GetDataFrame(carAhp)
#' ShowTable(carAhp)
#'
#' @docType package
#' @name ahp
NULL