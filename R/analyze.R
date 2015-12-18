
CalculateWeightContribution <- function(tr) {
  
  tr$Do(function(x) x$RemoveAttribute('weightContribution', FALSE))
  
  #calculate weight contribution for leaves
  tr$Do(function(x) x$weightContribution <- prod(x$Get("weight", traversal = "ancestor")),
        filterFun = isLeaf)
  
  #put into array on parent of leaves
  tr$Do(function(x) x$weightContribution <- sapply(x$children, function(x) x$weightContribution),
        filterFun = function(x) x$height == 2)
  
  #sum up (custom aggregation)
  tr$Do(function(x) x$weightContribution <- rowSums(sapply(x$children, function(x) c(x$weightContribution))),
        traversal = 'post-order',
        filterFun = function(x) x$height >= 3)
  
  
}


#' Calculates the contribution of criteria preferences
#' to the overall decision.
#' 
#' @param tr the calculated AHP data.tree
#' @return a data.frame containing the contribution of each criteria
#' 
#' @export
Analyse <- function(tr) {

  CalculateWeightContribution(tr)
  
  df <- do.call(ToDataFrameTree, 
                c(tr,
                  'name',
                  'level',
                  weight = function(x) sum(x$weightContribution),
                  'consistency',
                  GetWeightContributionV(names(sort( tr$weightContribution, decreasing = TRUE))),
                  filterFun = isNotLeaf))[,-1]
  
  return (df)
  
}



GetWeightContribution <- function(alternative, formatter = identity) {
  f <- function(node) {
    formatter(node$weightContribution[alternative])
  }
  return (f)
}

GetWeightContributionV <- Vectorize(GetWeightContribution, vectorize.args = 'alternative')
