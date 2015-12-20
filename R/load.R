
GetPreferences <- function(prefNode) {
  if (is.character(prefNode)) prefNode <- eval(parse(text = prefNode))
  prefs <- t(mapply(c, prefNode))
  prefs[,3] <- sapply(prefs[,3], FUN = function(x) eval(parse(text = x)))
  prefs <- as.data.frame(prefs, stringsAsFactors = FALSE)
  prefs[,3] <- as.numeric(prefs[,3])
  colnames(prefs) <- c('c1', 'c2', 'preference')
  return (prefs)
}


#' Load an ahp model from file
#' 
#' ahp files are in YAML format, and they are self-contained, fully specified ahp problems. They contain two sections: alternatives and goal.
#' The goal section is a tree of criteria, each criteria having a name, a preference attribute, and possible child criteria or alternatives.
#' 
#' @param file The full path to the file to be loaded.
#' @return A \code{\link{data.tree}} containing the model specification.
#' 
#' @examples 
#' ahpFile <- system.file("extdata", "car.ahp", package="ahp")
#' 
#' #look at a sample file
#' cat(readChar(ahpFile, file.info(ahpFile)$size))
#' 
#' #load the file into R
#' carAhp <- LoadFile(ahpFile)
#'
#'@export
LoadFile <- function(file) {

  oMat <- yaml::yaml.load_file(file)
  
  tr <- FromListExplicit(oMat[["Goal"]], childrenName = "criteria")
  
  tr$Do(fun = function(x) x$preferences <- GetPreferences(x$preferences),
        filterFun = function(x) !is.null(x$preferences)
  )
  
  #TODO: test validity of tree
  
  t <- Traverse(tr, filterFun = function(x) !is.null(x$preferenceFunction))
  Do(t, fun = function(x) x$preferenceFunction <- eval(parse(text = x$preferenceFunction)))
  
  return (tr)
  
  
}