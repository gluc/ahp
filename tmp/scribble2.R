

library(ahp)

tr <- LoadAhpFile("inst/extdata/car.ahp")

CalculateAhp(tr)

#tree table
alternatives <- GetAlternativesNames(tr)

tr$Do(function(x) x$weightContribution <- prod(x$Get("weight", traversal = "ancestor")),
      filterFun = isLeaf)

#put into array
tr$Do(function(x) x$weightContribution <- sapply(x$children, function(x) x$weightContribution),
      filterFun = function(x) x$height == 2)

#sum up (custom aggregation)
tr$Do(function(x) x$weightContribution <- rowSums(sapply(x$children, function(x) c(x$weightContribution))),
      traversal = 'post-order',
      filterFun = function(x) x$height >= 3)

GetWeightContribution <- function(alternative, formatter = identity) {
  f <- function(node) {
    formatter(node$weightContribution[alternative])
  }
  return (f)
}

GetWeightContributionV <- Vectorize(GetWeightContribution, vectorize.args = 'alternative')




#print
dfp <- do.call(ToDataFrameTree, 
               c(tr,
                 weight = function(x) FormatPercent(sum(x$weightContribution), 1),
                 consistency = function(x) FormatPercent(x$consistency, 1),
                 `|` = function(x) '|',
                 GetWeightContributionV(names(sort( tr$weightContribution, decreasing = TRUE)), formatter = function(x) FormatPercent(x, 1)),
                 filterFun = isNotLeaf))


df <- do.call(ToDataFrameTree, 
              c(tr,
              'name',
              'level',
              weight = function(x) sum(x$weightContribution),
              'consistency',
              GetWeightContributionV(names(sort( tr$weightContribution, decreasing = TRUE))),
              filterFun = isNotLeaf))[,-1]

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



weightCol <- "honeydew3"
goodCol <- "white"
badCol <- "wheat2"
alternativeCol <- "thistle4"


cols <- 2*df[,-(1:4)]/max(df[,-(1:4)]) + df[,-(1:4)]/df$weight
#cols <- df[,-(1:4)]
cols$zero <- 0
cols$max <- max(cols)
cols <- t(apply(cols, MARGIN = 1, function(x) csscolor(gradient(x, "white", alternativeCol))))
cols <- cols[,1:(ncol(cols)-2)]
names(df)[1] <- " "

myFormatters <- vector("list", length(alternatives))
for(i in 1:length(myFormatters)) myFormatters[[i]] <- percent1
names(myFormatters) <- alternatives


myFormatters$weight <- ColorTileWithFormatting("white", weightCol, percent1)
myFormatters$consistency <- ConsistencyFormatter(goodCol, badCol, percent1)
myFormatters$` ` <- formatter("span", 
                              style = style(`white-space` = "nowrap",
                                              `text-align` = "left",
                                              float = "left",
                                              `font-weight` = "bold",
                                              `text-indent` = paste0((df$level-1), "em")
                              ))

for(a in alternatives) myFormatters[[a]] <- ColorTileRowWithFormatting(cols[,a], percent1)
                                  

formattable(df[ , -2], formatters = myFormatters)
                       

