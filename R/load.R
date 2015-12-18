library(yaml)
library(data.tree)




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
#' @param file the path and name of the file to load
#' @return a data.tree containing the model specification
#'
#'@export
LoadAhpFile <- function(file) {

  oMat <- yaml.load_file(file)
  
  tr <- FromListExplicit(oMat[["Goal"]], childrenName = "criteria")
  
  tr$Do(fun = function(x) x$preferences <- GetPreferences(x$preferences),
        filterFun = function(x) !is.null(x$preferences)
  )
  
  #TODO: test validity of tree
  
  t <- Traverse(tr, filterFun = function(x) !is.null(x$preferenceFunction))
  Do(t, fun = function(x) x$preferenceFunction <- eval(parse(text = x$preferenceFunction)))
  
  return (tr)
  
  
}