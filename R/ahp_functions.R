# Define methods to calculate and tweak AHP (Analytic Hierarchy Process)

RI <- function(n){ #see handout
  if (n==2) return (100)
  if (n==3) return (0.58)
  if (n==4) return (0.90)
  if (n==5) return (1.12)
  if (n==6) return (1.24)
  if (n==7) return (1.32)
  if (n==8) return (1.41)
  return (1)
}

#' Calculate the ahp priority weights from the AHP matrix.
#' 
#' For a comparison of different methods, see for example \bold{How to derive priorities in AHP: a comparative study}, 
#' by Alessio Ishizaka and Markus Lusti, as available here: http://eprints.port.ac.uk/9041/1/filetodownload,70633,en.pdf
#' 
#' @param mat The AHP preference matrix
#' @param allowedConsistency if the AHP consistency ratio is larger 
#' than this value, AHP is not applied and equal weights are returned.
#' @return the ahp preference weights
#' 
#' @export
PrioritiesFromPairwiseMatrixEigenvalues <- function(mat, allowedConsistency = 1) { 
  # weigthing vector
  eig <- eigen(mat, symmetric=FALSE)
  
  #consistency
  M22 = mat/kronecker(matrix(1, dim(mat)[1], 1), t(apply(mat, 2, sum)))
  w = apply(M22, 1, sum) / dim(mat)[1]
  lambdaMax <- max(Re(eig$values))
  CI = (lambdaMax - dim(mat)[1]) / (dim(mat)[1]-1)
  CR = CI / RI(dim(mat)[1])
  CR <- max(CR, 0) #due to numerical inprecision
  #consistency
  if (is.nan(CI) || CR < allowedConsistency) res <- (Re(eig$vectors[,1])/sum(Re(eig$vectors[,1]))) else res <- (matrix(1/dim(mat)[1],1,dim(mat)[1]))
  names(res) <- dimnames(mat)[[1]]
  list(priority = res, consistency = CR)
}

#' @rdname PrioritiesFromPairwiseMatrixEigenvalues
#' 
#' @export
PrioritiesFromPairwiseMatrixMeanNormalization <- function(mat) {
  priority <- rowMeans( mat / matrix(rep(colSums(mat), nrow(mat)), nrow = nrow(mat), byrow = TRUE))
  list(priority = priority, consistency = NA)
}


#' @rdname PrioritiesFromPairwiseMatrixEigenvalues
#' 
#' @export
PrioritiesFromPairwiseMatrixGeometricMean <- function(mat) {
  geometricMean <- apply(mat, MARGIN = 1, prod) ^ (1 / nrow(mat))
  priority <- geometricMean / sum(geometricMean)
  list(priority = priority, consistency = NA)
}


#' Create the AHP preference matrix from a dataframe containing
#' the pairwiswe preferences. 
#' 
#' @param preferenceCombinations a data.frame containing category or alternative
#' A in the first column, B in the second colum, and the preference in the third column.
#' @return an AHP preference matrix
#' 
#' @export
AhpMatrix <- function(preferenceCombinations) {
  cats <- unlist(unique(c(preferenceCombinations[,1], preferenceCombinations[,2])))
  mat <- matrix(1, nrow = length(cats), ncol = length(cats), byrow = TRUE, dimnames = list(cats, cats))
  for (i in 1:nrow(preferenceCombinations)) {
    mat[preferenceCombinations[[i,1]], preferenceCombinations[[i,2]]] <- preferenceCombinations[[i,3]]
    mat[preferenceCombinations[[i,2]], preferenceCombinations[[i,1]]] <- 1 / preferenceCombinations[[i,3]]
  }
  return(mat)
}


#' Converts a vector of scores into priority weights.
#' 
#' While pure AHP limits itself to pairwise preferences, scoring alternatives 
#' on an arbitrary scale is often much less time consuming in practice. This method
#' calculates the priority weight as \code{score / sum(scores)}
#' 
#' @param scores a vector of scores
#' @return a vector of priority weights
#' 
#' @export
PrioritiesFromScoresDefault <- function(scores) {
  return (scores / sum(scores))
}



