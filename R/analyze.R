


#' Converts the tree into a data.frame, containing all the weights contributions 
#' to the overall decision.
#' 
#' @param ahpTree the calculated AHP \code{data.tree}
#' @param decisionMaker the name of the decision maker. The default returns the joint decision.
#' @param variable the variable to display, i.e. either weightContribution (the default), priority, or score
#' @param sort sort either by priority according to the decision maker (the default), by totalPriority, or as originally specified (orig) 
#' @return a \code{data.frame} containing the contribution of each criteria
#' 
#' @export
Analyze <- function(ahpTree, 
                    decisionMaker = "Total", 
                    variable = c("weightContribution", "priority", "score"),
                    sort = c("priority", "totalPriority", "orig")) {
 
  
  df <- GetDataFrame(ahpTree, decisionMaker, variable, sort)
  df <- df[ , -c(2, 3)]
  if (!variable[1] == "score") percentColumns <- 2:ncol(df)
  else percentColumns <- c(2, ncol(df))
  for (i in percentColumns) df[ , i] <- percent1(df[ , i])
  return (df)
   
}
  

GetDataFrame <- function(ahpTree, 
                         decisionMaker = "Total", 
                         variable = c("weightContribution", "priority", "score"),
                         sort = c("priority", "totalPriority", "orig")) {
  
  if (sort[1] == "priority" || sort[1] == "totalPriority") ahpTree <- Clone(ahpTree)
  if (sort[1] == "priority") ahpTree$Sort(function(x) ifelse(x$isLeaf, x$position, x$parent$priority[decisionMaker, x$name]), decreasing = TRUE)
  else if (sort[1] == "totalPriority") ahpTree$Sort(function(x) ifelse(x$isLeaf, x$position, x$parent$priority[decisionMaker, x$name]), decreasing = TRUE)
                                        
  
  if (sort[1] == "priority") nms <- names(sort( ahpTree$weightContribution[decisionMaker, ], decreasing = TRUE))
  else if (sort[1] == "totalPriority") nms <- names(sort( ahpTree$weightContribution["Total", ], decreasing = TRUE))
  else nms <- names(ahpTree$weightContribution["Total", ])
  
  df <- do.call(ToDataFrameTree, 
                c(ahpTree,
                  'name',
                  'level',
                  Weight = function(x) sum(x$weightContribution[decisionMaker, ]),
                  Priority = function(x) ifelse(x$isRoot, 1, x$parent$priority[decisionMaker, x$name]),
                  GetVariableV(nms, 
                               decisionMaker = decisionMaker, 
                               variable = variable[1]),
                  Consistency = function(x) x$consistency[decisionMaker],
                  filterFun = isNotLeaf))

  names(df)[1] <- " "
  
  if (variable[1] == "priority") df <- df[ , -4]
  else df <- df[ , -5]
  
  return (df)
  
  
}



GetVariable <- function(alternative, decisionMaker, variable = "weightContribution", formatter = identity) {
  f <- function(node) {
          #print(paste0("Node: ", node$name, " DecisionMaker: ", decisionMaker, " Variable: ", variable))
          if (is.null(node[[variable]])) return (formatter(NA))
          if (! alternative %in% colnames(node[[variable]])) return (formatter(NA))
          if (! decisionMaker %in% rownames(node[[variable]])) return (formatter(NA))
          formatter(node[[variable]][decisionMaker, alternative])
       }
  return (f)
}

GetVariableV <- Vectorize(GetVariable, vectorize.args = 'alternative')
