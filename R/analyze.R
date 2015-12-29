


#' Converts the tree into a data.frame, containing all the weights contributions 
#' to the overall decision.
#' 
#' @param ahpTree the calculated AHP \code{\link{data.tree}}
#' @param decisionMaker the name of the decision maker. The default returns the joint decision.
#' @return a \code{data.frame} containing the contribution of each criteria
#' 
#' @export
GetDataFrame <- function(ahpTree, decisionMaker = "Total") {

  df <- do.call(ToDataFrameTree, 
                c(ahpTree,
                  Weight = function(x) sum(x$weightContribution[decisionMaker, ]),
                  GetWeightContributionV(names(sort( ahpTree$weightContribution["Total", ], decreasing = TRUE)), decisionMaker),
                  Consistency = function(x) x$consistency[decisionMaker],
                  filterFun = isNotLeaf))

  names(df)[1] <- " "
  
  for (i in 2:ncol(df)) df[ , i] <- ahp:::percent1(df[ , i])
  
  return (df)
  
}



GetWeightContribution <- function(alternative, decisionMaker, formatter = identity) {
  f <- function(node) {
    formatter(node$weightContribution[decisionMaker, alternative])
  }
  return (f)
}

GetWeightContributionV <- Vectorize(GetWeightContribution, vectorize.args = 'alternative')
