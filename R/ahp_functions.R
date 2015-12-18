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

#' Calculate the ahp weights from the symmetric AHP matrix
#' 
#' @param mat The symmetric AHP preference matrix
#' @return the ahp preference weights
#' 
#' @export
Ahp <- function(mat, allowedConsistency = 1){ 
  # weigthing vector
  eig <- eigen(mat, symmetric=FALSE)
  
  #consistency
  M22 = mat/kronecker(matrix(1, dim(mat)[1], 1), t(apply(mat, 2, sum)))
  w = apply(M22, 1, sum) / dim(mat)[1]
  lambdaMax <- max(Re(eig$values))
  CI = (lambdaMax - dim(mat)[1]) / (dim(mat)[1]-1)
  CR = CI / RI(dim(mat)[1])
  
  #consistency
  if (is.nan(CI) || CR < allowedConsistency) res <- (Re(eig$vectors[,1])/sum(Re(eig$vectors[,1]))) else res <- (matrix(1/dim(mat)[1],1,dim(mat)[1]))
  names(res) <- dimnames(mat)[[1]]
  list(ahp = res, consistency = CR)
}

#' Create the AHP pairwise preference matrix from vectors
#' 
#' @details Creates and fills a AHP conform matrix given the category vectors cat1, cat2, and the preference vector pref.
#' @param cat1 A vector of n category names
#' @param cat2 A vector of n category names
#' @param pref A vector of preferences
#' @examples
#' categories <- combn(c('red','blue','green'),2)
#' cat1 <- categories[1,]
#' cat2 <- categories[2,]
#' pref <- c(1,5,4)
#' AhpMatrix(cat1, cat2, pref)
#' @seealso \code{\link{ahp}}
#' @export
AhpMatrix <- function(preferenceCombinations){
	cats <- unique(c(levels(preferenceCombinations$a), levels(preferenceCombinations$b)))
	mat <- matrix(1, nrow = length(cats), length(cats), byrow = TRUE, dimnames = list(cats, cats))
	for (i in 1:dim(preferenceCombinations)[1])
	{
		mat[as.character(preferenceCombinations[i,1]), as.character(preferenceCombinations[i,2])] <- preferenceCombinations[i,3]
		mat[as.character(preferenceCombinations[i,2]), as.character(preferenceCombinations[i,1])] <- 1/ preferenceCombinations[i,3]
	}
	
	return(mat)

}


