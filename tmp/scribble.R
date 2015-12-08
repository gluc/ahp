yamlStr <- 'mytree:
  preferences:
    type: matrix
    cols:     [Accord,       Civic,        Prius]
    data:
      - Accord: [532.80990646, 0.0,          342.49522219]
      - Civic:  [0.0,          532.93344713, 3.88792491  ]
      - Prius:  [1/2,          0.0,          1.0   ]'


library(yaml)
oMat <- yaml.load(yamlStr, handlers=list(seq=function(x) { as.character(x) }))


tr <- as.Node(oMat$mytree, nodeName = "mytree")

prefNode <- tr

GetTreeMatrix <- function(prefNode) {
  prefs <-  t(sapply(prefNode$data$fields, function(x) prefNode$data[[x]]))
  colnames(prefs) <- prefNode$cols
  prefs <- apply(prefs, MARGIN = c(1,2) , FUN = function(x) eval(parse(text = x)))
  return (prefs)
}


IsMatrix <- function(x) {
  if (!x$isLeaf) return (FALSE)
  return (x$type == "matrix")
}

ReplaceMatrix <- function(x) {
  parent <- x$parent
  parent$RemoveChild(x$name)
  parent[[x$name]] <- GetTreeMatrix(x)
}


t <- Traverse(tr, filterFun = IsMatrix)
Do(t, ReplaceMatrix)
