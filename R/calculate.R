

#'Calculate the Ahp tree
#'
#'@param ahpTree a data.tree tree containing the Ahp model specification
#'
#'@import data.tree
#'
#'@export
Calculate <- function(ahpTree) {
  
  # go from
  # 1. function to pairwise
  # 2. pairwise to matrix
  # 3. matrix to weight
  # 4. from weight into weight matrix
  
  
  prefTrees <- ahpTree$Get(attribute = function(x) x$preferences, filterFun = function(x) !is.null(x$preferences))
  
  
  for(prefTree in prefTrees) {
  
    # 1. from function to pairwise
    prefTree$Do(fun = function(x) {
                        pw <- x$parent$AddChild(name = "pairwise") 
                        pw$preferences <- GetPairwiseFromFunction(x)
                      },
                filterFun = function(x) x$name == "function")
    
    # 2. from pairwise to matrix
    prefTree$Do(fun = function(x) {
                        pw <- x$parent$AddChild(name = "matrix") 
                        pw$preferences <- AhpMatrix(x$preferences)
                      },
                filterFun = function(x) x$name == "pairwise")
    
    # 3. from matrix to weight
    prefTree$Do(fun = function(x) {
                        pw <- x$parent$AddChild(name = "weight") 
                        ahp <- CalculateAhpMatrix(x$preferences)
                        pw$preferences <- ahp$ahp
                        pw$consistency <- ahp$consistency
                      },
                filterFun = function(x) x$name == "matrix")
    
    #after this step, all preferences should have a weight
    
    # 4. put weight and consistency into children
    for (child in prefTree$myParent$children) {
      w <- prefTree$Get(function(x) x$preferences[[child$name]], filterFun = function(x) x$name == "weight")
      names(w) <-  names(prefTree$children)
      child$weight <- w
      
    }
    
    
    # consistency
    cons <- prefTree$Get(function(x) x$consistency, filterFun = function(x) x$name == "weight")
    names(cons) <-  names(prefTree$children)
    prefTree$myParent$consistency <- cons
    
    
 
  }
  
  
  
  #print(ahpTree, consistency = function(x) FormatPercent(x$consistency), weight = function(x) FormatPercent(x$weight))
  CalculateWeightContribution(ahpTree)
  
  CalculateTotalWeightContribution(ahpTree)
  CalculateTotalConsistency(ahpTree)
  
}



GetAlternativesNames <- function(ahpTree) {
  unique(ahpTree$Get("name", filterFun = isLeaf))
}


#create table from function
GetPairwiseFromFunction <- function(node) {
  #combn(names(node$children), m = 2, FUN = function(x) node$preferenceFunction(node[[x[[1]]]], node[[x[[2]]]]))
  prefs <- data.frame(t(combn(node$root$myParent$children, m = 2, FUN = function(x) c(x[[1]]$name, x[[2]]$name, node$preferences(x[[1]], x[[2]])))),
                      stringsAsFactors = FALSE)
  prefs[,3] <- as.numeric(prefs[,3])
  colnames(prefs) <- c('c1', 'c2', 'preference')
  return (prefs)
}


GetPreference <- function(node, type, decisionMaker, attribute = "preferences") {
  node$preferences[[decisionMaker]][[type]][[attribute]]
}

GetDecisionMakers <- function(ahpTree) {
  decisionMakers <- unique(c(ahpTree$Get(attribute = function(x) names(x$preferences$children), filterFun = function(x) !is.null(x$preferences))))
  return (decisionMakers)
}



CalculateWeightContribution <- function(ahpTree) {
  
  #TODO: add whenever newest version of data.tree is on CRAN
  #ahpTree$Do(function(x) x$RemoveAttribute('weightContribution', FALSE))
  
  #calculate weight contribution for alternatives
  dm <- GetDecisionMakers(ahpTree)
  ahpTree$Do(function(x) {
          x$weightContribution <- apply(sapply(x$Get("weight", traversal = "ancestor")[-length(dm)], cbind), MARGIN = 1, prod)
          names(x$weightContribution) <- dm
        },
        filterFun = isLeaf)
  
  
  #put into matrix on parent of alternatives
  ahpTree$Do(function(x) x$weightContribution <- sapply(x$children, function(x) x$weightContribution),
        filterFun = function(x) x$height == 2)
  
  #sum up for criteria (custom aggregation)
  ahpTree$Do(function(x) x$weightContribution <- Reduce('+', Get(x$children, "weightContribution", simplify = FALSE)),
        traversal = 'post-order',
        filterFun = function(x) x$height >= 3)
  
  
  
  
}


CalculateTotalWeightContribution <- function(ahpTree) {
  
  ahpTree$Do(function(x) {
                dm <- GetAttribute(x, attribute = "decision-makers", inheritFromAncestors = TRUE, nullAsNa = FALSE)
                if (is.null(dm)) dm <- rep(1/nrow(x$weightContribution), nrow(x$weightContribution))
                totals <- t(t(x$weightContribution) %*% dm)
                rownames(totals) <- "Total"
                x$weightContribution <- rbind(x$weightContribution, Total = totals)
              },
             filterFun = isNotLeaf
  )
  
}


CalculateTotalConsistency <- function(ahpTree) {
  
  ahpTree$Do(function(x) x$consistency <- c(x$consistency, Total = max(x$consistency, na.rm = TRUE)),
             filterFun = isNotLeaf
             )
  
}
