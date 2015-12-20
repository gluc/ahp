


#' Displays the AHP analysis in form of an html table, with gradient
#' colors and nicely formatted.
#' 
#' @param tr The calculated ahp tree
#' @param weightColor The name of the color to be used to emphasize weights of categories. See \code{color} for a list of possible colors.
#' @param consistencyColor The name of the color to be used to highlight bad consistency
#' @param alternativeColor The name of the color used to highlight big contributors to alternative choices.
#' @return a \code{\link{formattable}} object which, in most environments, will be displayed as an HTML table
#' 
#' @import formattable
#' @export 
ShowTable <- function(tr, 
                      weightColor = "honeydew3", 
                      consistencyColor = "wheat2",
                      alternativeColor = "thistle4") {

  df <- do.call(ToDataFrameTree, 
                c(tr,
                  'name',
                  'level',
                  Weight = function(x) sum(x$weightContribution),
                  GetWeightContributionV(names(sort( tr$weightContribution, decreasing = TRUE))),
                  Consistency = function(x) x$consistency,
                  filterFun = isNotLeaf))[,-1]
  
  alternatives <- names(df)[-c(1:3, ncol(df))]
  cols <- 2*df[,alternatives]/max(df[,alternatives]) + df[,alternatives]/df$Weight
  #cols <- df[,-(1:4)]
  cols$zero <- 0
  cols$max <- max(cols)
  cols <- t(apply(cols, MARGIN = 1, function(x) csscolor(gradient(x, "white", alternativeColor))))
  cols <- cols[,1:(ncol(cols)-2)]
  
  names(df)[1] <- " "
  
  myFormatters <- vector("list", length(alternatives))
  names(myFormatters) <- alternatives
  for(a in alternatives) myFormatters[[a]] <- ColorTileRowWithFormatting(cols[,a], percent1)
  
  myFormatters$Weight <- ColorTileWithFormatting("white", weightColor, percent1)
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


#' @import formattable
percent1 <- function(x) percent(x, digits = 1)

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
