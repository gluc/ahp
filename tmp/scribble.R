yamlStr <- 'preferences:
  type: matrix
  rows:     [Accord,       Civic,        Prius]
  data:
    Accord: [532.80990646, 0.0,          342.49522219]
    Civic:  [0.0,          532.93344713, 3.88792491  ]
    Prius:  [0.0,          0.0,          1.0         ]'




library(yaml)
oMat <- yaml.load(yamlStr)
m <- oMat$preferences

GetYAMLMatrix <- function(m) {
  prefs <- matrix(unlist(m$data), byrow = TRUE, nrow = length(m$data))
  rownames(prefs) <- m$rows
  colnames(prefs) <- names(m$data)
  return (prefs)
}

GetYAMLMatrix(m)
