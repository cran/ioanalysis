import.total <- function(io){
  if(class(io) != "InputOutput") stop('io should be of "InputOutput" class. See ?as.inputoutput')
  Z <- io$Z
  RS_label <- io$RS_label
  regions <- unique(RS_label[, 1])
  # Getting to work
  import <- rep(NA, dim(Z)[1])
  if(!exists("io$M") & length(regions) == 1) stop("There is only one region and no imports. Check io$M")
  check <- 0
  if(exists("io$M")){
    check <- 1
    M <- io$M
    if(dim(M)[2] > 1){
      one <- matrix(rep(1, dim(M)[2]))
      M <- M %*% one
    }
  } else if(length(regions) > 1){
    for(r in 1:length(regions)){
      i <- which(RS_label[, 1] == regions[r])
      import[i] <- colSums(Z[-i, i])
    }
  }
  if(check == 1){
    import <- import + M
  }
  matrix(import)
}
