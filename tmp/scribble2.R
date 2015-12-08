
yamlStr <- 'Choose the best car for the Jones family:
  preferences:
    type: matrix
    cols:       [Cost, Safety, Style, Capacity]
    data:
      Cost:     [1   , 3     , 7    , 3       ]
      Safety:   [1/3 , 1     , 9    , 1       ]
      Style:    [1/7 , 1/9   , 1    , 7       ]
      Capacity: [1/3 , 1     , 1/7  , 1       ] 
  Cost:
    preferences:
      type: matrix
      cols:               [Purchase Price, Fuel Cost, Maintenance Cost, Resale Value]
      data:
        Purchase Price:   [1             , 2        , 5               , 3           ]
        Fuel Cost:        [1/2           , 1        , 2               , 2           ]
        Maintenance Cost: [1/5           , 1/2      , 1               , 1/2         ]
        Resale Value:     [1/3           , 1/2      , 2               , 1           ] 
    Purchase Price: 
      Alternatives: [Accord Sedan, Accord Hybrid, Pilot SUV, CR-V SUV, Element SUV, Odyssey Minivan]
    Fuel Costs:
      Alternatives: [Accord Sedan, Accord Hybrid, Pilot SUV, CR-V SUV, Element SUV, Odyssey Minivan]
    Maintenance Costs:
      Alternatives: [Accord Sedan, Accord Hybrid, Pilot SUV, CR-V SUV, Element SUV, Odyssey Minivan]
    Resale Value:
      Alternatives: [Accord Sedan, Accord Hybrid, Pilot SUV, CR-V SUV, Element SUV, Odyssey Minivan]
  Safety:
    Alternatives: [Accord Sedan, Accord Hybrid, Pilot SUV, CR-V SUV, Element SUV, Odyssey Minivan]
  Style:
    Alternatives: [Accord Sedan, Accord Hybrid, Pilot SUV, CR-V SUV, Element SUV, Odyssey Minivan]
  Capacity:
    Cargo Capacity:
      Alternatives: [Accord Sedan, Accord Hybrid, Pilot SUV, CR-V SUV, Element SUV, Odyssey Minivan]
    Passenger Capacity:
      Alternatives: [Accord Sedan, Accord Hybrid, Pilot SUV, CR-V SUV, Element SUV, Odyssey Minivan]'


library(yaml)
library(data.tree)
oMat <- yaml.load(yamlStr, handlers=list(seq=function(x) { as.character(x) }))

tr <- as.Node(oMat[[1]], nodeName = names(oMat))


GetTreeMatrix <- function(prefNode) {
  prefs <-  t(sapply(prefNode$data$fields, function(x) prefNode$data[[x]]))
  colnames(prefs) <- prefNode$cols
  prefs <- apply(prefs, MARGIN = c(1,2) , FUN = function(x) eval(parse(text = x)))
  return (prefs)
}


IsMatrix <- function(x) {
  if (x$isLeaf || x$isRoot) return (FALSE)
  return (!is.null(x$type) && x$type == "matrix")
}

ReplaceMatrix <- function(x) {
  parent <- x$parent
  parent$RemoveChild(x$name)
  parent[[x$name]] <- GetTreeMatrix(x)
}


t <- Traverse(tr, filterFun = IsMatrix)
Do(t, ReplaceMatrix)
