


#' Displays the AHP analysis in form of an html table, with gradient
#' colors and nicely formatted.
#' 
#' @param weightColor The name of the color to be used to emphasize weights of categories. See \code{color} for a list of possible colors.
#' @param consistencyColor The name of the color to be used to highlight bad consistency
#' @param alternativeColor The name of the color used to highlight big contributors to alternative choices.
#' @return AnalyzeTable returns a \code{\link{formattable}} data.frame object which, in most environments, will be displayed as an HTML table
#' 
#' @rdname Analyze
#' @import formattable
#' @export 
AnalyzeTable <- function(ahpTree,
                         decisionMaker = "Total",
                         variable = c("weightContribution", "priority", "score"),
                         sort = c("priority", "totalPriority", "orig"),
                         pruneFun = function(node, decisionMaker) TRUE,
                         weightColor = "honeydew3",
                         consistencyColor = "wheat2",
                         alternativeColor = "thistle4") {
  
                           

  df <- GetDataFrame(ahpTree = ahpTree, 
                     decisionMaker = decisionMaker, 
                     variable = variable, 
                     sort = sort, 
                     pruneFun = pruneFun)
  df <- df[ , -1]
  
  alternatives <- names(df)[-c(1:3, ncol(df))]
  dfw <- df[ , alternatives, drop = FALSE]
  
  dfw[is.na(dfw)] <- 1
  #cols <- 2 * dfw/max(dfw) + dfw/df[ , 3]
  #cols <- df[,-(1:4)]
  if (variable[1] == "weightContribution") cols <- dfw/max(dfw)
  else cols <- dfw/apply(dfw, MARGIN = 1, FUN = max)
  
  cols$zero <- 0
  cols$max <- max(cols)
  cols <- t(apply(cols, MARGIN = 1, function(x) csscolor(gradient(x, "white", alternativeColor))))
  cols <- cols[,1:(ncol(cols)-2), drop = FALSE]
  
  names(df)[1] <- " "
  
  myFormatters <- vector("list", length(alternatives))
  names(myFormatters) <- alternatives
  alternativesFormatter <- percent1
  if (variable[1] == "score") alternativesFormatter <- identity
  for(a in alternatives) myFormatters[[a]] <- ColorTileRowWithFormatting(cols[,a], alternativesFormatter)
  
  
  myFormatters[[colnames(df)[3]]] <- ColorTileWithFormatting("white", weightColor, percent1)
  myFormatters$Consistency <- ConsistencyFormatter("white", consistencyColor, percent1)
  myFormatters$` ` <- formatter("span", 
                                style = style(`white-space` = "nowrap",
                                              `text-align` = "left",
                                              float = "left",
                                              `font-weight` = "bold",
                                              `text-indent` = paste0((df$level-1), "em")
                                ))
  
  
  
  
  formattable(df[ , -2], formatters = myFormatters)

}


#' @param node the \code{Node}
#' @param minWeight prunes the nodes whose weightContribution is smaller than the minWeight
#' 
#' @return the Prune methods must return \code{TRUE} for nodes to be kept, \code{FALSE} for nodes to be pruned
#' 
#' @rdname Analyze
#' @export
PruneByCutoff <- function(node, decisionMaker, minWeight = 0) {
  res <- sum(node$weightContribution[decisionMaker, ]) >= minWeight
  return (res)
}



#' @param levelsToPrune cuts the \code{n} hightest levels of the ahp tree 
#' 
#' @rdname Analyze
#' @export
PruneLevels <- function(node, decisionMaker, levelsToPrune = 0) {
  return (node$level <= (node$root$height - levelsToPrune - 1))
}



#' @import formattable
percent1 <- function(x) {
  if (all(is.na(x))) return (x)
  percent(x, digits = 1)
}

ColorTileWithFormatting <- function(c1, c2, format) {
  
  formatter("span", 
            style = x ~ style(
              display = "block", 
              padding = "0 4px", 
              `border-radius` = "4px", 
              `background-color` = csscolor(gradient(x, c1, c2))),
            x ~ format(x)
  )
}


ColorTileRowWithFormatting <- function(cols, format) {
  
  formatter("span", 
            style = style(
              display = "block", 
              padding = "0 4px", 
              `border-radius` = "4px", 
              `background-color` = cols),
            x ~ format(x)
  )
}



ConsistencyFormatter <- function(c1, c2, format) {
  
  formatter("span", 
            style = x ~ style(
              display = "block", 
              padding = "0 4px", 
              `border-radius` = "4px", 
              `background-color` = csscolor(gradient(pmin(x, 0.1), c1, c2))),
            x ~ icontext(ifelse(x > 0.1 , "exclamation-sign", NA), format(x))
  )
}
