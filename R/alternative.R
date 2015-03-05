

#' @include alternative_node.R
NULL


#' AHP alternative
#' 
#' @description An alternative in the AHP problem.
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @usage Alternative$new()
#' @keywords ahp
Alternative <- R6Class("Alternative",
                        lock = FALSE,
                        public = list(
                          name = "",
                          alternativeNodes = list(),
                          
                          initialize=function(name, ...) {
                            if (!missing(name)) {
                              self$name <- name
                            }
                            
                            invisible (self)
                          }
                        ) 
                  
 
)






#' Constructor for a list of alternatives
#' 
#' @seealso \code{\link{Alternative}}
#' @export
AlternativesList <- function(...) {
  alternatives <- list(...)
  class(alternatives) <- append("AlternativesList", class(alternatives))
  return (alternatives)
}


#' @export
print.AlternativesList <- function(alternatives, ...) {
  print(as.data.frame(alternatives, formatAlternative = data.tree:::FormatPercent, formatDecimal = data.tree:::PrintFixedDecimal))
}


#' Convert an AlternativesList object into a dataframe.
#' This is particularly useful for printing the results of an AHP study.
#' 
#' @export
as.data.frame.AlternativesList <- function(alternatives, row.names = NULL, optional = FALSE, ...) {
  
  el <- list(...) 
  if (!is.null(el$formatAlternative)) {
    fAlt <- el$formatAlternative
  } else {
    fAlt <- function(x) x
  }

  if (!is.null(el$formatDec)) {
    fDec <- el$formatDec
  } else {
    fDec <- function(x) x
  }
  
  
  goal <- alternatives[[1]]$alternativeNodes[[1]]$root
  
  o <- order(sapply(alternatives, function(x) goal$GetAlternativePriority(x$name)), decreasing = TRUE)
  alternatives <- alternatives[o]
  
  cols <- lapply(alternatives, function(x) goal$Get("GetAlternativePriority", x$name, format = fAlt))
  names(cols) <- names(alternatives)
  
  l <- list(
                isLeaf = goal$Get("isLeaf"),
                consistency = goal$Get("consistency", format = fDec),
                globalPriority = goal$Get("globalPriority", format = fDec)
  )
  
  cols <- append(l, cols)
  
  
  
  df <- do.call(goal$ToDataFrame, cols)
  df <- df[!df$isLeaf,-2]
  
  return (df)  
}


#' @export
priorities.AlternativesList <- function(alternatives) {
  sapply(alternatives, function(y) sum(sapply(y$alternativeNodes, function(x) x$globalPriority)))
}


#' @export
plotHeatmap <- function(alternativesList) {
  # alternatives %>% as.data.frame %>% set_rownames(.$levelName) %>% subset(select = 4:9) %>% as.matrix -> hm
  df <- as.data.frame(alternatives)
  rownames(df) <- df$levelName
  df <- df[,4:9]
  hm <- as.matrix(df)
  heatmap(hm, Rowv = NA, Colv = NA, revC = TRUE)
}
