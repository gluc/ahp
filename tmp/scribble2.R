

library(yaml)
library(data.tree)
library(ahp)
library(formattable)

oMat <- yaml.load_file('tmp/ahldef.yaml')

tr <- FromListExplicit(oMat[["Goal"]], childrenName = "criteria")

#prefNode <- tr$preferences
#prefNode <- tr$Cost$`Fuel Costs`$preferences
#class(prefNode)

#parse preference table
GetPreferences <- function(prefNode) {
  if (is.character(prefNode)) prefNode <- eval(parse(text = prefNode))
  prefs <- t(mapply(c, prefNode))
  prefs[,3] <- sapply(prefs[,3], FUN = function(x) eval(parse(text = x)))
  prefs <- as.data.frame(prefs, stringsAsFactors = FALSE)
  prefs[,3] <- as.numeric(prefs[,3])
  colnames(prefs) <- c('c1', 'c2', 'preference')
  return (prefs)
}


tr$Do(fun = function(x) x$preferences <- GetPreferences(x$preferences),
      filterFun = function(x) !is.null(x$preferences)
      )

#TODO: test validity of tree

t <- Traverse(tr, filterFun = function(x) !is.null(x$preferenceFunction))
Do(t, fun = function(x) x$preferenceFunction <- eval(parse(text = x$preferenceFunction)))

#create table from function
GetPreferences <- function(node) {
  #combn(names(node$children), m = 2, FUN = function(x) node$preferenceFunction(node[[x[[1]]]], node[[x[[2]]]]))
  prefs <- data.frame(t(combn(node$children, m = 2, FUN = function(x) c(x[[1]]$name, x[[2]]$name, node$preferenceFunction(x[[1]], x[[2]])))),
                      stringsAsFactors = FALSE)
  prefs[,3] <- as.numeric(prefs[,3])
  colnames(prefs) <- c('c1', 'c2', 'preference')
  return (prefs)
}

Do(t, fun = function(x) x$preferences <- GetPreferences(x))

#create matrix from table

AhpMatrix <- function(preferenceCombinations) {
  cats <- unlist(unique(c(preferenceCombinations$c1, preferenceCombinations$c2)))
  mat <- matrix(1, nrow = length(cats), ncol = length(cats), byrow = TRUE, dimnames = list(cats, cats))
  for (i in 1:nrow(preferenceCombinations)) {
    mat[preferenceCombinations[[i,1]], preferenceCombinations[[i,2]]] <- preferenceCombinations[[i,3]]
    mat[preferenceCombinations[[i,2]], preferenceCombinations[[i,1]]] <- 1 / preferenceCombinations[[i,3]]
  }
  return(mat)
}

t <- Traverse(tr, filterFun = isNotLeaf)
Do(t, fun = function(x) x$preferenceMatrix <- AhpMatrix(x$preferences))

#print preference matrices
#tr$Do(function(x) { print(x$name); print(AhpMatrix(x$preferences))}, filterFun = isNotLeaf)

#calculate weights
DoAhp <- function(node) {
  ahp <- Ahp(node$preferenceMatrix)
  node$consistency <- ahp$consistency
  for (cat in names(ahp$ahp)) node[[cat]]$weight <- ahp$ahp[[cat]]
}

tr$weight <- 1

Do(t, fun = function(x) DoAhp(x))

print(tr, consistency = function(x) FormatPercent(x$consistency), weight = function(x) FormatPercent(x$weight))


#tree table
alternatives <- names(oMat$Alternatives)

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
#GetWeightContributionV(tr, names(oMat$Alternatives))



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

myFormatters <- vector("list", length(oMat$Alternatives))
for(i in 1:length(myFormatters)) myFormatters[[i]] <- percent1
names(myFormatters) <- names(oMat$Alternatives)


myFormatters$weight <- ColorTileWithFormatting("white", weightCol, percent1)
myFormatters$consistency <- ConsistencyFormatter(goodCol, badCol, percent1)
myFormatters$` ` <- formatter("span", 
                              style = style(`white-space` = "nowrap",
                                              `text-align` = "left",
                                              float = "left",
                                              `font-weight` = "bold",
                                              `text-indent` = paste0((df$level-1), "em")
                              ))

for(a in names(oMat$Alternatives)) myFormatters[[a]] <- ColorTileRowWithFormatting(cols[,a], percent1)
                                  

formattable(df[ , -2], formatters = myFormatters)
                       

