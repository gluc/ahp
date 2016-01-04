


#' Converts the tree into a data.frame, containing all the weights contributions 
#' to the overall decision.
#' 
#' @param ahpTree the calculated AHP \code{data.tree}
#' @param decisionMaker the name of the decision maker. The default returns the joint decision.
#' @return a \code{data.frame} containing the contribution of each criteria
#' 
#' @export
GetDataFrame <- function(ahpTree, decisionMaker = "Total", variable = c("weightContribution", "priority", "score")) {

  df <- do.call(ToDataFrameTree, 
                c(ahpTree,
                  Weight = function(x) sum(x$weightContribution[decisionMaker, ]),
                  GetVariableV(names(sort( ahpTree$weightContribution["Total", ], decreasing = TRUE)), 
                                         decisionMaker = decisionMaker, 
                                         variable = variable[1]),
                  Consistency = function(x) x$consistency[decisionMaker],
                  filterFun = isNotLeaf))

  names(df)[1] <- " "
  
  
  if (!variable[1] == "score") percentColumns <- 2:ncol(df)
  else percentColumns <- c(2, ncol(df))
  for (i in percentColumns) df[ , i] <- percent1(df[ , i])
  
  return (df)
  
}



GetVariable <- function(alternative, decisionMaker, variable = "weightContribution", formatter = identity) {
  f <- function(node) {
    if (is.null(node[[variable]])) return (formatter(NA))
    if (! alternative %in% colnames(node[[variable]])) return (formatter(NA))
    if (! decisionMaker %in% rownames(node[[variable]])) return (formatter(NA))
    formatter(node[[variable]][decisionMaker, alternative])
  }
  return (f)
}

GetVariableV <- Vectorize(GetVariable, vectorize.args = 'alternative')
