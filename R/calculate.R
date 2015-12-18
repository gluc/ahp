

#'Calculate the Ahp tree
#'
#'@param tr a data.tree tree containing the Ahp model specification
#'
#'@import data.tree
#'
#'@export
Calculate <- function(tr) {
  t <- Traverse(tr, filterFun = function(x) !is.null(x$preferenceFunction))
  
  Do(t, fun = function(x) x$preferences <- GetPreferencesFromFunction(x))

  #create matrix from table
  t <- Traverse(tr, filterFun = isNotLeaf)
  Do(t, fun = function(x) x$preferenceMatrix <- AhpMatrix(x$preferences))
  
  tr$weight <- 1
  
  Do(t, fun = function(x) DoAhp(x))
  
  #print(tr, consistency = function(x) FormatPercent(x$consistency), weight = function(x) FormatPercent(x$weight))
  
}



#' Get the names of the alternatives in the tree
#' 
#' @param tr The ahp tree
#' @return a character vector containing the names of all the alternatives
#' 
#' @export
GetAlternativesNames <- function(tr) {
  unique(tr$Get("name", filterFun = isLeaf))
}


#create table from function
GetPreferencesFromFunction <- function(node) {
  #combn(names(node$children), m = 2, FUN = function(x) node$preferenceFunction(node[[x[[1]]]], node[[x[[2]]]]))
  prefs <- data.frame(t(combn(node$children, m = 2, FUN = function(x) c(x[[1]]$name, x[[2]]$name, node$preferenceFunction(x[[1]], x[[2]])))),
                      stringsAsFactors = FALSE)
  prefs[,3] <- as.numeric(prefs[,3])
  colnames(prefs) <- c('c1', 'c2', 'preference')
  return (prefs)
}


#print preference matrices
#tr$Do(function(x) { print(x$name); print(AhpMatrix(x$preferences))}, filterFun = isNotLeaf)

#calculate weights
DoAhp <- function(node) {
  ahp <- Ahp(node$preferenceMatrix)
  node$consistency <- ahp$consistency
  for (cat in names(ahp$ahp)) node[[cat]]$weight <- ahp$ahp[[cat]]
}

