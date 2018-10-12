agg.region <- function(io, regions, newname = "newname"){
  # Preliminaries
  if(class(io) != "InputOutput") stop('io should be of "InputOutput" class. See ?as.inputoutput')
  if(missing(regions)) stop("Please specify which regions you wish to aggregate")
  if(missing(newname)) stop("Please specify the new name for the aggregated sectors.")
  RS_label <- io$RS_label
  f_label <- io$f_label
  # Checking to see if the final demand is aggregated
  check <- 0
  if(dim(f_label)[2] == 1){
    check <- 1
  }
  # Checking for valid regions
  if(class(regions) == "factor"){
    regions <- as.character(regions)
  }
  if(class(regions) == "character"){
    for(k in 1:length(regions)){
      if(!regions[k] %in% RS_label[, 1]) stop(paste(regions[k], "is not a region in RS_label. Check spelling, capitalization, and punctuation."))
    }
  }
  else if(class(regions) == "numeric" | class(regions) == "integer"){
    region <- unique(RS_label[, 1])
    regions <- region[regions]
  }
  # Checking to each region for aggregation has the same regions
  sectors <- unique(RS_label[which(RS_label[, 1] %in% regions), 2])
  if(check.RS(io) == FALSE){
    stop("Each region must have the same sectors in the same order. See ?check.RS")
  }
  # Aggregating regions
  n <- dim(RS_label)[1]
  I <- diag(n)
  sectors <- unique(RS_label[, 2])
  for(s in 1:length(sectors)){
    i <- which(RS_label[, 1] %in% regions & RS_label[, 2] %in% sectors[s])
    temp <- matrix(I[i[-1], ], ncol = n)
    one <- matrix(1, ncol = dim(temp)[1], nrow = 1)
    temp <- one %*% temp
    I[i[1], ] <- I[i[1], ] + temp
  }
  # Adjusting the labels
  RS_label[which(RS_label[,1] %in% regions[1]), 1] <- newname
  if(check == 0){
    f_label[1, which(f_label[1,] %in% regions[1])] <- newname
  }
  if(!is.null(io$E)){
    E_label <- io$E_label
    E_label[1, which(E_label[1,] %in% regions[1])] <- newname
  }
  regions <- regions[-1]
  i <- which(RS_label[, 1] %in% regions)
  RS_label <- RS_label[-i, ]
  S <- I[-i, ]
  if(check == 0){
    i <- which(f_label[1, ] %in% regions)
    f_label <- f_label[, -i]
  }
  if(!is.null(io$E)){
    i <- which(E_label[1, ] %in% regions)
    E_label <- matrix(E_label[, -i], nrow = 2)
  }
  ######################################
  ## creating a new InputOuput object ##
  ######################################
  if(dim(S)[2] > 1500){
    cat("\n\n    Calculating new InputOuput object...\n\n")
  }
  n <- dim(S)[1]
  IO <- NULL
  IO$Z <- S %*% io$Z %*% t(S)
  IO$RS_label <- RS_label
  if(check == 0){
    IO$f <- S %*% io$f
    IO$f_label <- f_label
  } else if(check == 1){
    IO$f <- io$f
    IO$f <- io$f_label
  }
  if(!is.null(io$E)){
    IO$E <- S %*% io$E
    IO$E_label <- E_label
  }
  IO$X <- S %*% io$X
  if(!is.null(io$V)){
    IO$V <- io$V %*% t(S)
    IO$V_label <- io$V_label
  }
  if(exists("io$M")){
    IO$M <- io$M %*% t(S)
    IO$M_label <- io$M_label
  }
  if(!is.null(io$fV)){
    IO$fV <- io$fV
    IO$fV_label <- io$fV_label
  }
  xhat <- matrix(1/IO$X, ncol = n, nrow = n)
  IO$A <- IO$Z * xhat
  xhat <- matrix(1/IO$X, ncol = n, nrow = n, byrow = TRUE)
  IO$B <- IO$Z * xhat
  IO$L <- leontief.inv(A = IO$A)
  IO$G <- ghosh.inv(B = IO$B)
  class(IO) <- "InputOutput"
  IO
}
