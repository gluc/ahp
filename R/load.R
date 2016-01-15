



#' Load an ahp model from file
#' 
#' ahp files are in YAML format, and they are self-contained, fully specified ahp problems. 
#' They contain two sections: \bold{alternatives} and \bold{goal}. 
#' 
#' The \bold{alternatives} section contains a list of alternatives, where each alternative
#' may have a number of attributes.
#' 
#' The \bold{goal} section is a tree of criteria, 
#' each criteria having a \code{name}, a \code{preferences}  
#' attribute, and possible child criteria or alternatives.
#' 
#' To look at a sample file, type, see examples below or type \code{vignette("examples", package = "ahp")}. To learn
#' the details about the ahp file format, type \code{vignette("file-format", package = "ahp")}.
#' 
#' @param ahpFile The full path to the file to be loaded, or a connection.
#' @return A \code{\link{data.tree}} containing the model specification.
#' 
#' @examples 
#' ahpFile <- system.file("extdata", "car.ahp", package="ahp")
#' 
#' #look at a sample file
#' cat(readChar(ahpFile, file.info(ahpFile)$size))
#' 
#' #load the file into R
#' carAhp <- Load(ahpFile)
#'
#' @export
Load <- function(ahpFile) {
  if (is.character(ahpFile))
    fileContent <- readChar(ahpFile, file.info(ahpFile)$size)
  return (LoadString(fileContent))
}


#' @param ahpString A character string to be loaded.
#' 
#' @rdname Load
#' @export
LoadString <- function(ahpString) {
  oMat <- NULL
  tryCatch({
    oMat <- yaml::yaml.load(ahpString)
  },
  error = function(e) {
    stop(paste0("Could not load ahp model. File must be a valid YAML file. Exception caught when parsing YAML file: ", e))
  })
  
  tryCatch({
    if (!"Version" %in% names(oMat)) stop("Could not load ahp model. Could not find Version")
    if (!"Goal" %in% names(oMat)) stop("Could not load ahp model. Model does not contain a Goal!")
    tr <- FromListExplicit(oMat[["Goal"]])
    
    fct <- "DoLoad_"
    fct <- paste0(fct, oMat$Version)
    
    do.call(fct, list(tr))
    
    return (tr)
  },
  error = function(e) {
    stop(paste0("Could not load ahp model. Exception caught when converting into a data.tree: ", e))
  })
}


DoLoad_1 <- function(tr) {
  
  trav <- Traverse(tr, filterFun = function(x) !(is.null(x$preferences) && is.null(x$preferenceFunction)))

  Do(trav, 
     fun = function(x) {
       if (!is.null(x$preferences)) mypref <- x$preferences
       x$preferences <- list()
       if (exists("mypref", inherits = FALSE)) x$preferences$pairwise <- mypref
       if (!is.null(x$preferenceFunction)) x$preferences$pairwiseFunction <- x$preferenceFunction
     }
    )
  
  tr$Do(fun = function(x) x$preferences <- GetPreferences_2(x),
    filterFun = function(x) !is.null(x$preferences)
  )


}









DoLoad_2 <- function(tr) {
  tr$Do(fun = function(x) x$preferences <- GetPreferences_2(x),
        filterFun = function(x) !is.null(x$preferences)
  )
  
  if (!is.null(tr$`decision-makers`)) {
    wc <- unlist(tr$`decision-makers`)
    wc <- sapply(wc, FUN = function(x) eval(parse(text = x)))
    tr$`decision-makers` <- wc
  }
  
}



GetPreferences_2 <- function(criteriaNode) {
  newPreferences <- Node$new(name = "preferences")
  newPreferences$myParent <- criteriaNode
  
  #shortcut if single decision maker
  if (length(names(criteriaNode$preferences)) == 1 &&
      names(criteriaNode$preferences) %in% c('pairwise', 'pairwiseFunction', 'priority', 'score', 'scoreFunction')) {
    criteriaNode$preferences <- list(DecisionMaker = criteriaNode$preferences)
  }
  
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
    } else if (type == "pairwiseFunction" || type == "scoreFunction") {
      prefs <- eval(parse(text = prefNode))
    } else if (type == "priority" || type == "score") {
      prefs <- unlist(prefNode)
      prefs <- sapply(prefs, FUN = function(x) eval(parse(text = x)))
    } else {
      stop(paste0("Unknown preference type <", type, "> for node ", criteriaNode$pathString, "! Must be <pairwise>, <pairwiseFunction>, <priority>, <score> or <scoreFunction>."))
    }
    
    prefTr <- newPreferences$AddChild(name = decisionMaker)
    prefType <- prefTr$AddChild(name = type)
    prefType$preferences <- prefs
    
  }
  return (newPreferences)
}


