as.inputoutput <- function(Z, RS_label,
                           f, f_label,
                           E, E_label,
                           X,
                           V, V_label,
                           M, M_label,
                           fV, fV_label,
                           A, B, L, G)
{
  # Creating the object
  io <- NULL
  n <- dim(Z)[1]
  # Intermediate Sales Matrix
  if(dim(Z)[1] != dim(Z)[2]) stop("The intermediate transaction (Z) matrix needs to be a square matrix")
  if(dim(Z)[1] != length(X)) stop("Check dimensions/length; Z should be a nxn matrix and X should be a nx1 vector")
  io$Z <- as.matrix(Z)
  # Region Sector label
  if(dim(RS_label)[1] != dim(io$Z)[1]) stop("Row dimension of Z and RS_label must match")
  if(dim(RS_label)[2] != 2) stop("RS_label must have the first column of regions and second regions sectors")
  io$RS_label <- as.character(RS_label[, 1])
  io$RS_label <- cbind(io$RS_label, as.character(RS_label[, 2]))

  # Final demand matrix -- could be a matrix or a vector
  if(!missing(f)){
    if( is.null(dim(f)) ){
      if( length(f) != dim(io$Z) ) stop("Column dimension of f and Z must match")
      io$f <- matrix(f, ncol = 1)
    } else {
      if(dim(f)[1] != length(X)) stop("Column dimension of f and Z must match")
      io$f <- as.matrix(f)
    }
    # Final demand label
    if( missing("f_label")){stop("If the final demand matrix (f) is supplied, a label must match")  }
    if( is.null(dim(f_label)) ){
      if( length(f_label) != dim(io$f)[2]) stop("Column dimension of f and f_label must match")
      io$E_label <- matrix(f_label, ncol = 1)
    } else {
      if( dim(f_label)[2] != dim(io$f)[2] ) stop("Column dimension of f and f_label must match")
      io$f_label <- f_label
    }
  } else if(missing(f)){
    cat("\n  Final Demand matrix (f) was not provided. Calculating aggregate Final Demand... \n\n")
    one <- matrix(rep(1,n))
    io$f <- X - io$Z %*% one
    io$f_label <- matrix(c("aggregate", "aggregate"))
  }

  # Export matrix -- could be a matrix or a vector
  if(!missing(E)){
    if( is.null(dim(E)) ){
      if(length(E) != length(X)) stop("Column dimension of E and Z must match")
      io$E <- matrix(E)
    } else {
    if(dim(E)[1] != length(X)) stop("Check dimensions/length; E should be a nxm matrix")
      io$E <- as.matrix(E)
    }
    # Export label
    if( missing("E_label")){stop("If the export matrix (E) is supplied, a label must match")  }
    if( is.null(dim(E_label)) ){
      if( length(E_label) != dim(io$E)[2]) stop("Column dimension of E and E_label must match")
      io$E_label <- matrix(E_label, ncol = 1)
    } else {
      if( dim(E_label)[2] != dim(io$E)[2] ) stop("Column dimension of E and E_label must match")
      io$E_label <- E_label
    }
  }

  # Total Production -- a vector
  io$X <- matrix(X, ncol = 1)

  # Value added -- could be a matrix or a vector
  if(!missing(V)){
    if( is.null(dim(V)) ){
      if(length(V) != length(X)) stop("Row dimension of V and Z must match")
      io$V <- matrix(V, nrow = 1)
    } else{
      if(dim(V)[2] != length(X)) stop("Column dimension of V and Z must match")
      io$V <- as.matrix(V)
    }
    # Value added label
    if(missing(V_label)) {stop("If the value added matrix (V) is supplied, a label must match")  }
    if( is.null(dim(V_label)) ){
      if( length(V_label) != dim(io$V)[1]) stop("Row dimension of V and V_label must match")
      io$V_label <- matrix(V_label, ncol = 1)
    } else {
      if( dim(V_label)[1] != dim(io$V)[1] ) stop("Row dimension of V and V_label must match")
      io$V_label <- V_label
    }
  }

  # Import matrix
  if(!missing(M)){
    if(missing(M_label)){stop("If the import matrix (M) is supplied, a label must match")  }
    if( is.null(dim(M)) ){
      if(length(M) != length(X)) stop("Column dimension of M and Z must match")
      io$M <- matrix(M, nrow = 1)
      check <- 1
    } else {
      if(dim(M)[2] != length(X)) stop("Column dimension of M and Z must match")
      io$M <- as.matrix(M)
    }
    # Import label
    if(check == 1){
      if( !missing("M_label")){
        if( is.null(dim(M_label)) ){
          if( length(M_label) != dim(io$M)[1]) stop("Row dimension of M and M_label must match")
          io$M_label <- matrix(M_label, nrow = 1)
        }
      }
    } else {
    if( !missing("M_label")){
      if( is.null(dim(M_label)) ){
        if( length(M_label) != dim(io$M)[1]) stop("Row dimension of M and M_label must match")
        io$M_label <- matrix(M_label, nrow = 1)
      } else {
        if( dim(M_label)[1] != dim(io$M)[1] ) stop("Row dimension of M and M_label must match")
        io$M_label <- M_label
      }
    }
    }
  }
  
  # Creating the fV matrix
  if(!missing(fV)){
    if(dim(fV)[2] != dim(f)[2]) stop("The number of columns of fV must match f. It's fine to have NAs in fV")
    io$fV <- fV
    if(missing(fV_label)) stop("If the fV matrix is provided, there must be an fV_label")
    if(dim(fV)[1] != length(fV_label)) stop("The number of rows of fV and fV_label must match")
    io$fV_label <- matrix(fV_label)
  }
  
  # To avoid dividing by zero when calculating technical coefficient matrix
  i <- which(X == 0)
  X[i] <- 1
  # Technical input Coefficient Matrix
  if(missing(A)){
    n <- length(X)
    xhat <- matrix(1/X, ncol = n, nrow = n)
    io$A <- Z * xhat
  } else {
    io$A <- A
  }

  # Technical Output Coefficient Matrix
  if(missing(B)){
    n <- length(X)
    xhat <- matrix(1/X, ncol = n, nrow = n, byrow = TRUE)
    io$B <- Z * xhat
  } else {
    io$B <- B
  }

  # Creating the Leontief inverse
  if(missing(L)){
    io$L <- leontief.inv(A = io$A)
  }

  # Creating the Ghoshian inverse
  if(missing(G)){
    io$G <- ghosh.inv(B = io$B)
  }

  # Finalizing the object
  class(io) <- "InputOutput"
  io
}
