

library(yaml)
library(data.tree)
library(ahp)
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

GetPath <- function(n) {
    f <- function(x) {
            if (x$level > n)  return (x$path[[n]])
            return (NA)
         }
    return (f)
}

GetPathV <- Vectorize(GetPath)

pathArgs <- GetPathV(2:(tr$height - 1))
names(pathArgs) <- paste0('l', 2:(tr$height - 1))

df <- do.call(ToDataFrameTable, c(tr, pathArgs, 'name', weight = function(x) prod(x$Get('weight', traversal = 'ancestor'))))

library(treemap)
treemap(df, index = c('name', names(pathArgs)), vSize = 'weight', type = "value")

