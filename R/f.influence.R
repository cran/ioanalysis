f.influence <- function(ioname, i, j){
  if(class(ioname) != "character") stop('ioname should be the name of your "InputOutput" object as saved in your workspace.')
  if(!exists(ioname)) stop('ioname should be the name of your "InputOutput" object as saved in your workspace.')
  if(length(i) != length(j)) stop("i must be the same length as j")
  n <- dim(get(ioname)[["L"]])[1]
  if(length(i) >= n) stop("Field of Influence is only defined up to n-1 terms, where n = #sectors * #regions")
  # nth order field of influence
  if(length(i) > 1){
    k <- length(i)
    A <- matrix(0, nrow = n, ncol = n)
    for(s in 1:k){
      for(r in 1:k){
        Lij <- get(ioname)[["L"]][i[s], j[r]]
        A <- 1/(k-1)*(  (-1)^(s+r+1) * Lij * f.influence(ioname, i[-s], j[-r])) + A
      }
    }
    return(A)
  }
  # 1st order field of influence
  else if(length(i) == 1){
    L.j <- get(ioname)[["L"]][, j]
    Li. <- get(ioname)[["L"]][i, ]
    matrix(L.j, ncol = 1) %*% matrix(Li., nrow = 1)
  }
}
