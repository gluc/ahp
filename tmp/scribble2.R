

library(yaml)
library(data.tree)
oMat <- yaml.load_file('tmp/ahldef.yaml')

tr <- FromListExplicit(oMat[["Goal"]], childrenName = "criteria")

#prefNode <- tr$preferences
#class(prefNode)

#parse preference table
GetPreferences <- function(prefNode) {
  if (is.character(prefNode)) prefNode <- eval(parse(text = prefNode))
  prefs <- t(mapply(c, prefNode))
  prefs[,3] <- sapply(prefs[,3], FUN = function(x) eval(parse(text = x)))
  prefs <- as.data.frame(prefs)
  colnames(prefs) <- c('c1', 'c2', 'preference')
  return (prefs)
}


tr$Do(fun = function(x) x$preferences <- GetPreferences(x$preferences),
      filterFun = function(x) !is.null(x$preferences)
      )

#parse functions
GetPreferencesFromFunction <- function(prefFunString) {
  eval(parse(text = prefFunString))
}


tr$Do(fun = function(x) x$preferenceFunction <- GetPreferencesFromFunction(x$preferenceFunction), 
      filterFun = function(x) !is.null(x$preferenceFunction)
      )

#TODO: create table from function
#TODO: create matrix from table
#TODO: test validity of tree