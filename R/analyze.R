


#' Converts the tree into a data.frame, containing all the weights contributions 
#' to the overall decision.
#' 
#' @param tr the calculated AHP data.tree
#' @return a data.frame containing the contribution of each criteria
#' 
#' @export
GetDataFrame <- function(tr) {

  df <- do.call(ToDataFrameTree, 
                c(tr,
                  Weight = function(x) sum(x$weightContribution),
                  GetWeightContributionV(names(sort( tr$weightContribution, decreasing = TRUE))),
                  Consistency = function(x) x$consistency,
                  filterFun = isNotLeaf))

  names(df)[1] <- " "
  
  for (i in 2:ncol(df)) df[ , i] <- percent1(df[ , i])
  
  return (df)
  
}



GetWeightContribution <- function(alternative, formatter = identity) {
  f <- function(node) {
    formatter(node$weightContribution[alternative])
  }
  return (f)
}

GetWeightContributionV <- Vectorize(GetWeightContribution, vectorize.args = 'alternative')
