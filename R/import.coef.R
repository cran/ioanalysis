import.coef <- function(io, region){
  if(class(io) != "InputOutput") stop('io should be of "InputOutput" class. See ?as.inputoutput')
  if(check.RS(io) == FALSE) stop("Regions must have same sectors. Try ?locate.mismatch, ?agg.sector, ?easy.select")
  if(!class(region) %in% c("numeric", "integer")) stop("Region must be an integer")
  if(length(region) != 1) stop("Region can only be one number")
  A <- io$A
  M <- io$M
  if(!is.null(M)){
    warning("\nWARNING: io$M exists. This means the import coefficient matrix may be biased\n\nCoefficient matrix is still calculated.")
  }
  RS_label <- io$RS_label
  regions <- unique(RS_label[, 1])
  if(length(regions) == 1) stop("Cannot calculate import coefficient matrix if there is only one region")
  j <- which(RS_label[, 1] == regions[region])
  import <- matrix(0, ncol = length(j), nrow = length(j))
  regions <- regions[-region]
  for(r in 1:length(regions)){
    i <- which(RS_label[, 1] == regions[r])
    import <- import + A[i, j]
  }
  return(import)
}
