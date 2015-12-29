
  
GetPreferences <- function(criteriaNode) {
  newPreferences <- Node$new(name = "preferences")
  newPreferences$myParent <- criteriaNode
  for(decisionMaker in names(criteriaNode$preferences)) {
    preferences <- criteriaNode$preferences[[decisionMaker]]
    #derive type
    type <- names(preferences)

    prefNode <- preferences[[type]]
    
    if (type == 'pairwise') {
      if (is.character(prefNode)) prefNode <- eval(parse(text = prefNode)) #to handle single pref
      prefs <- t(mapply(c, prefNode))
      prefs[,3] <- sapply(prefs[,3], FUN = function(x) eval(parse(text = x)))
      prefs <- as.data.frame(prefs, stringsAsFactors = FALSE)
      prefs[,3] <- as.numeric(prefs[,3])
      colnames(prefs) <- c('c1', 'c2', 'preference')
    } else if (type == "function") {
      prefs <- eval(parse(text = prefNode))
    } else if (type == "weight") {
      prefs <- unlist(prefNode)
      prefs <- sapply(prefs, FUN = function(x) eval(parse(text = x)))
    } else {
      stop(paste0("Unknown preference type <", type, "> for node ", criteriaNode$pathString, "! Must be <pairwise>, <function>, or <weight>."))
    }
    
    prefTr <- newPreferences$AddChild(name = decisionMaker)
    prefType <- prefTr$AddChild(name = type)
    prefType$preferences <- prefs
    
  }
  return (newPreferences)
}


#' Load an ahp model from file
#' 
#' ahp files are in YAML format, and they are self-contained, fully specified ahp problems. 
#' They contain two sections: \bold{alternatives} and \bold{goal}. 
#' 
#' The \bold{alternatives} section contains a list of alternatives, where each alternative
#' may have a number of attributes.
#' 
#' The \bold{goal} section is a tree of criteria, 
#' each criteria having a \code{name}, a \code{preferences} or a \code{preferenceFunction} 
#' attribute, and possible child criteria or alternatives.
#' 
#' To look at a sample file, type, see examples below.
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
  
  tr <- FromListExplicit(oMat[["Goal"]])
  
  tr$Do(fun = function(x) x$preferences <- GetPreferences(x),
        filterFun = function(x) !is.null(x$preferences)
  )
  
  if (!is.null(tr$`decision-makers`)) {
    wc <- unlist(tr$`decision-makers`)
    wc <- sapply(wc, FUN = function(x) eval(parse(text = x)))
    tr$`decision-makers` <- wc
  }
  
  #TODO: test validity of tree
  
  return (tr)
  
  
}