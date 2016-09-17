

#'Calculate the Ahp tree
#'
#'@param ahpTree a data.tree tree containing the Ahp model specification
#'@param pairwiseFun lets you overwrite the function that is used to calculate the priorities from 
#'the pairwise preference matrix.
#'@param scoresFun lets you overwrite the function that is used to calculate the priorites from scores.
#'
#'@import data.tree
#'
#'@export
Calculate <- function(ahpTree, 
                      pairwiseFun = PrioritiesFromPairwiseMatrixEigenvalues, 
                      scoresFun = PrioritiesFromScoresDefault) {
  
  # go from
  # 1. function to pairwise
  # 2. pairwise to matrix
  # 3. matrix to priority/weight
  # 4. score function to score
  # 5. score to priority
  # 6. from priority/weight into weight matrix
  
  
  prefTrees <- ahpTree$Get(attribute = function(x) x$preferences, filterFun = function(x) !is.null(x$preferences))
  
  
  for(prefTree in prefTrees) {
  
    # 1. from function to pairwise
    prefTree$Do(
      fun = function(x) {
        pw <- x$parent$AddChild(name = "pairwise") 
        pw$preferences <- GetPairwiseFromFunction(x)
      },
      filterFun = function(x) x$name == "pairwiseFunction"
    )
    
    # 2. from pairwise to matrix
    prefTree$Do(
      fun = function(x) {
        pw <- x$parent$AddChild(name = "pairwiseMatrix") 
        pw$preferences <- AhpMatrix(x$preferences)
      },
      filterFun = function(x) x$name == "pairwise"
    )
    
    # 3. from matrix to priority
    prefTree$Do(
      fun = function(x) {
        pw <- x$parent$AddChild(name = "priority") 
        ahp <- pairwiseFun(x$preferences)
        pw$preferences <- ahp$priority
        pw$consistency <- ahp$consistency
      },
      filterFun = function(x) x$name == "pairwiseMatrix"
    )
    
    
    # 4. from scoreFunction to score
    prefTree$Do(
      fun = function(x) {
        pw <- x$parent$AddChild(name = "score") 
        pw$preferences <- GetScoreFromFunction(x)
      },
      filterFun = function(x) x$name == "scoreFunction"
    )
    
    # 5. from score to priority
    prefTree$Do(
      fun = function(x) {
        pw <- x$parent$AddChild(name = "priority") 
        pw$preferences <- scoresFun(x$preferences)
      },
      filterFun = function(x) x$name == "score"
    )
    
    #after this step, all preferences should have a priority
    
    # Calculate Total priority
    CalculateTotalPriority(prefTree)
    
    # 5. put priority, score and consistency into children
  


    # consistency
    cons <- prefTree$Get(function(x) x$consistency, filterFun = function(x) x$name == "priority")
    names(cons) <-  names(prefTree$children)
    prefTree$myParent$consistency <- cons

  }
  
  SetVariable(ahpTree, "priority")
  SetVariable(ahpTree, "score")
  
  CalculateWeightContribution(ahpTree)
  CalculateTotalConsistency(ahpTree)
  PutContributionIntoPreferences(ahpTree)
  
}

SetVariable <- function(ahpTree, variable) {
  ahpTree$Do(
    function(x) {
      prefTree <- x$preferences
      prios <- prefTree$Get( "preferences", filterFun = function(x) x$name == variable)
      if (!is.null(prios)) {
        colnames(prios) <- prefTree$Get( function(x) x$parent$name, filterFun = function(x) x$name == variable)
        x[[variable]] <- t(prios)
      }
    },
    filterFun = function(x) !is.null(x$preferences)
  )
}



GetAlternativesNames <- function(ahpTree) {
  unique(ahpTree$Get("name", filterFun = isLeaf))
}


#' Create table from function
#' @param node The node
#' @importFrom utils combn
GetPairwiseFromFunction <- function(node) {
  #combn(names(node$children), m = 2, FUN = function(x) node$preferenceFunction(node[[x[[1]]]], node[[x[[2]]]]))
  prefs <- data.frame(t(combn(node$root$myParent$children, m = 2, FUN = function(x) c(x[[1]]$name, x[[2]]$name, node$preferences(x[[1]], x[[2]])))),
                      stringsAsFactors = FALSE)
  prefs[,3] <- as.numeric(prefs[,3])
  colnames(prefs) <- c('c1', 'c2', 'preference')
  return (prefs)
}


GetScoreFromFunction <- function(node) {
  #combn(names(node$children), m = 2, FUN = function(x) node$preferenceFunction(node[[x[[1]]]], node[[x[[2]]]]))
  alternatives <- node$root$myParent$children
  
  scores <- sapply(alternatives, function(x) node$preferences(x), simplify = "array")
  
  return (scores)
}


GetPreference <- function(node, type, decisionMaker, attribute = "preferences") {
  node$preferences[[decisionMaker]][[type]][[attribute]]
}

GetDecisionMakers <- function(ahpTree) {
  decisionMakers <- unique(c(ahpTree$Get(attribute = function(x) names(x$preferences$children), filterFun = function(x) !is.null(x$preferences))))
  decisionMakers <- decisionMakers[decisionMakers!="Total"]
  return (decisionMakers)
}

CalculateTotalPriority <- function(prefTree) {
  priorityList <- prefTree$Get("preferences", filterFun = function(x) x$name == "priority", simplify = FALSE)
  mat <- sapply(priorityList, cbind)
  colnames(mat) <- names(prefTree$children)
  rownames(mat) <- names(priorityList[[1]])
  dm <- GetAttribute(prefTree$myParent, attribute = "decision-makers", inheritFromAncestors = TRUE, nullAsNa = FALSE)
  if (is.null(dm)) dm <- rep(1/ncol(mat), ncol(mat))
  totalPriorities <- c(mat %*% dm)
  names(totalPriorities) <- rownames(mat)
  tp <- prefTree$AddChild(name = "Total")
  tpp <- tp$AddChild(name = "priority")
  tpp$preferences <- totalPriorities
  
}

CalculateWeightContribution <- function(ahpTree) {
  
  #TODO: add whenever newest version of data.tree is on CRAN
  #ahpTree$Do(function(x) x$RemoveAttribute('weightContribution', FALSE))
  
  #calculate weight contribution for alternatives
  
  ahpTree$Do(
    function(x) {
      weights <- sapply(x$Get(function(x) x$parent$priority[ , x$name ], traversal = "ancestor")[-x$level], cbind)
      if (!is.matrix(weights)) weights <- matrix(weights, nrow = 1)
      x$weightContribution <- apply(weights, MARGIN = 1, FUN = prod)
      names(x$weightContribution) <- names(x$parent$preferences$children)
    },
    filterFun = isLeaf
  )
  
  
  #put into matrix on parent of alternatives
  ahpTree$Do(
    function(x) {
      x$weightContribution <- sapply(x$children, function(x) x$weightContribution)
      if (!is.matrix(x$weightContribution)) {
        x$weightContribution <- matrix(x$weightContribution, nrow = 1)
      }
      rownames(x$weightContribution) <- names(x$preferences$children)
      colnames(x$weightContribution) <- names(x$children)
    },
    filterFun = function(x) x$height == 2
  )
  
  #sum up for criteria (custom aggregation)
  ahpTree$Do(
    function(x) x$weightContribution <- Reduce('+', Get(x$children, "weightContribution", simplify = FALSE)),
    traversal = 'post-order',
    filterFun = function(x) x$height >= 3
  )
  
  

  
  
  
}



CalculateTotalConsistency <- function(ahpTree) {

    ahpTree$Do(
      function(x) {
        x$consistency["Total"] <- ifelse (!all(is.na(x$consistency)), max(x$consistency, na.rm = TRUE), NA)
        x$preferences$Total$priority$consistency <- x$consistency["Total"]
      },
      filterFun = isNotLeaf
    )

}




PutContributionIntoPreferences <- function(ahpTree) {
  #put into preferences
  ahpTree$Do(
    function(x) {
      #browser()
      wc <- x$weightContribution
      cons <- x$consistency
      for(dm in rownames(wc)) {
        wcn <- x$preferences[[dm]]$AddChild(name = "weightContribution")
        wcn$preferences <- c(wc[dm, ])
        #names(wcn$preferences) <- rownames(wc)
      }
      
    }
  )
}
