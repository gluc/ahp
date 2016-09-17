


#' Analyze your AHP model
#' 
#' Converts the calculated AHP tree into a \code{data.frame} or an HTML table, containing all the weight contributions or
#' priorities to/of the overall decision. You can also sort and filter the output.
#' 
#' @param ahpTree the calculated AHP \code{data.tree}
#' @param decisionMaker the name of the decision maker. The default returns the joint decision.
#' @param variable the variable to display, i.e. either weightContribution (the default), priority, or score
#' @param sort sort either by priority according to the decision maker (the default), by totalPriority, or as originally specified (orig) 
#' @param pruneFun use this to filter the what rows are shown in the analysis
#' \code{pruneFun} must be a function taking a \code{Node} as its first argument, and the \code{decisionMaker} as its second
#' argument. The default (\code{NULL}) returns the full AHP tree
#' @return Analyze returns a \code{data.frame} containing the contribution of each criteria
#' 
#' 
#' @examples 
#' ahpFile <- system.file("extdata", "car.ahp", package="ahp")
#' carAhp <- Load(ahpFile)
#' Calculate(carAhp)
#' Analyze(
#'    carAhp, 
#'    pruneFun = function(x, decisionMaker) {
#'      PruneLevels(x, decisionMaker, 1) && PruneByCutoff(x, decisionMaker, minWeight = 0.05)
#'    }
#' )
#'    
#'    
#' ahpFile <- system.file("extdata", "vacation.ahp", package="ahp")
#' vacationAhp <- Load(ahpFile)
#' Calculate(vacationAhp)
#' AnalyzeTable(
#'    vacationAhp,
#'    decisionMaker = "Kid",
#'    variable = "score",
#'    sort = "totalPriority"
#' )
#' 
#' @export
Analyze <- function(ahpTree, 
                    decisionMaker = "Total", 
                    variable = c("weightContribution", "priority", "score"),
                    sort = c("priority", "totalPriority", "orig"),
                    pruneFun = function(node, decisionMaker) TRUE) {
 
  
  df <- GetDataFrame(ahpTree, decisionMaker, variable, sort, pruneFun)
  df <- df[ , -c(2, 3)]
  if (!variable[1] == "score") percentColumns <- 2:ncol(df)
  else percentColumns <- c(2, ncol(df))
  for (i in percentColumns) df[ , i] <- percent1(df[ , i])
  return (df)
}
  


GetDataFrame <- function(ahpTree, 
                         decisionMaker = "Total", 
                         variable = c("weightContribution", "priority", "score"),
                         sort = c("priority", "totalPriority", "orig"),
                         pruneFun = function(node, decisionMaker) TRUE) {
  
  dms <- GetDecisionMakers(ahpTree)
  if (!class(ahpTree)[1] == "Node") stop("Argument ahpTree must be a data.tree structure")
  if (!(decisionMaker == "Total" || decisionMaker %in% dms)) stop(paste0("decisionMaker ", decisionMaker, " is not a decision maker of ahpTree"))
  if (!variable[1] %in% c("weightContribution", "priority", "score")) stop(paste0("variable must be weightContribution, priority, or score, but is ", variable))
  if (!sort[1] %in% c("priority", "totalPriority", "orig")) stop(paste0("sort must be priority, totalPriority, or orig, but is ", sort))
  if (length(formals(pruneFun))!=2) stop(paste0("pruneFun must have two arguments: node and decisionMaker"))  
  
  if (sort[1] == "priority" || sort[1] == "totalPriority") ahpTree <- Clone(ahpTree)
  if (sort[1] == "priority") Sort(ahpTree, function(x) ifelse(x$isLeaf, x$position, x$parent$priority[decisionMaker, x$name]), decreasing = TRUE)
  else if (sort[1] == "totalPriority") Sort(ahpTree, function(x) ifelse(x$isLeaf, x$position, x$parent$priority[decisionMaker, x$name]), decreasing = TRUE)
                                        
  
  if (sort[1] == "priority") nms <- names(sort( ahpTree$weightContribution[decisionMaker, ], decreasing = TRUE))
  else if (sort[1] == "totalPriority") nms <- names(sort( ahpTree$weightContribution["Total", ], decreasing = TRUE))
  else nms <- names(ahpTree$weightContribution["Total", ])
  
  if (decisionMaker == "Total" && length(dms) == 1) decisionMaker <- "DecisionMaker" #otherwise score is not shown
  
  df <- do.call(ToDataFrameTree, 
                c(ahpTree,
                  'name',
                  'level',
                  Weight = function(x) sum(x$weightContribution[decisionMaker, ]),
                  Priority = function(x) ifelse(x$isRoot, 1, x$parent$priority[decisionMaker, x$name]),
                  GetVariableV(nms, 
                               decisionMaker = decisionMaker, 
                               variable = variable[1]),
                  Inconsistency = function(x) x$consistency[decisionMaker],
                  pruneFun = function(x) !x$isLeaf && pruneFun(x, decisionMaker)
                  )
                )

  names(df)[1] <- " "
  
  if (variable[1] == "priority") df$Weight <- NULL
  else df$Priority <- NULL
  
  if (all(is.na(df$Inconsistency))) df$Inconsistency <- NULL
  
  
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
