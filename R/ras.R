ras <- function(io, x1, u1, v1, tol, maxiter, type, verbose = FALSE){
  # Preliminary checks
  if("InputOutput" %in% class(io)){
    A <- io$A
  } else {
    if(dim(io)[1] != dim(io)[2]) stop("Matrix of technical input coefficients (A) must be a square.")
    A <- io
  }
  n <- dim(A)[1]
  if(length(x1) != n) stop("x1 must be same length as the dimension of A")
  if(length(u1) != n) stop("u1 must be same length as the dimension of A")
  if(length(v1) != n) stop("v1 must be same length as the dimension of A")
  if(missing(tol)){
    tol <- 0.001
  }
  if(missing(maxiter)){
    maxiter <- 10000
  }
  if(missing(type)){
    type <- "o"
  }
  x1 <- matrix(x1)
  u1 <- matrix(u1)
  v1 <- matrix(v1)
  ###############
  ## rasSAFras ##
  ###############
  for(i in 1:maxiter){
    # Odd n
    u <- A %*% x1
    # preventing 0/0
    j <- which(u == 0)
    u[j] <- 1
    r <- u1/u
    R <- matrix(r, ncol = n, nrow = n)
    A1 <- R * A
    # Even n
    v <- t( t(x1) %*% A1 )
    # Preventing 0/0
    j <- which(v == 0)
    v[j] <- 1
    s <- v1/v
    S <- matrix(s, ncol = n, nrow = n, byrow = TRUE)
    A2 <- A1 * S
    # Checking convergence
    check <- A2 - A
    closeness <- norm(check, type = type)
    if(verbose == TRUE){
      cat(paste("  Iteration:", i, "  norm:", closeness, "\n"))
    }
    if(closeness < tol) break
    A <- A2
  }
  if(i == maxiter){
    warning("\n\n  No convergence. Maximum Number of iterations reached. Consider increasing the number of iterations.")
  }
  if(verbose == FALSE){
    cat(paste("\n\n  Number of iterations:", i, "\n\n"))
  }
  A2
}
