disaggregate = function(io, X = list(), U = list(), V = list(), new.regions, check = TRUE){
  # Checking for correct inputs
  if(class(io) != "InputOutput") stop("io must be of class InputOutput. See ?as.inputoutput")
  if(class(X) != 'list') stop('X must be a list over each regions total output')
  if(class(U) != 'list') stop('U must be a list over each regions row sums of intermediate transactions (Z)')
  if(class(V) != 'list') stop('V must be a list over each regions col sums of intermediate transactions (Z)')
  
  # Checking for consistent inputs
  if(length(X) != length(new.regions)) stop('X and new.regions must have the same number of regions')
  if(length(X) != length(U)) stop('X and U must have the same number of regions')
  if(length(X) != length(V)) stop('X and V must have the same number of regions')
  R = length(X)
  if(R > 2){
    reduce = TRUE
  } else {
    reduce = FALSE
  }
  if(R == 1) stop('Cannot disaggregate to less than 2 regions. Check length of (X)')
  
  
  # Grabbing what we need
  A        = io$A
  Z        = io$Z
  RS_label = io$RS_label
  r = length(unique(RS_label[, 1]))
  if(r > 1) stop('Can only disaggregate single region input-output systems. Consider ?agg.region')
  s = length(unique(RS_label[, 2]))
  
  
  # Checking for internal consistent inputs
  for(i in 1:R){
    if(length(X[[i]]) != s) stop(paste('X[[', i, ']] does not have the same number of sectors as (io)', sep = ''))
    if(length(U[[i]]) != s) stop(paste('U[[', i, ']] does not have the same number of sectors as (io)', sep = ''))
    if(length(V[[i]]) != s) stop(paste('V[[', i, ']] does not have the same number of sectors as (io)', sep = ''))
  }
  

 # Checking X and row & col sums add up to original
 # NOTE: This check allows for ALMOST perfect matches
 u.check = rowSums(Z)
 v.check = colSums(Z)
 if(check == TRUE){
   if(check.RS(io) == FALSE) stop('io must have the same sectors for all regions. See ?check.RS')
   # Check that total production per sector is consistent
   x.check = io$X
   for(i in 1:R){
     x.check = x.check - X[[i]]
   }
   if( sum(abs(x.check)) > 1) stop('Total production for all orignial sectors does not equal the provided total production
                           \nset check = FALSE to disable this check')
   # Check the rowsums are consistent
   for(i in 1:R){
     u.check = u.check - U[[i]]
   }
   if( sum(abs(u.check)) > 1) stop('Orignial row sums of (Z) do not match provided (U)
                            \nset check = FALSE to disable this check')
   # Check the colsums are consistent
   for(i in 1:R){
     v.check = v.check - V[[i]]
   }
   if( sum(abs(v.check)) > 1) stop('Orignial column sums of (Z) do not match provided (V)
                            \nset check = FALSE to disable this check')
 }

  
  # Calculating location quotients for the whole system.
  lq      = vector('list', R)
  lqtilde = lq
  X.tilde  = lq 
  bot     = io$X/sum(io$X)
  for(i in 1:R){
    lq[[i]] = matrix(X[[i]]/sum(X[[i]]) / bot)
    lq[[i]][lq[[i]] >= 1] = 1
    lqtilde[[i]] = matrix(Reduce('+', X[(setdiff(1:R, i))]) / sum(Reduce('+', X[(setdiff(1:R, i))])) / bot )
    lqtilde[[i]][lqtilde[[i]] > 1] = 1
    X.tilde[[i]]  = matrix(Reduce('+', X[setdiff(1:R, i)]))
  }
  # Building the new A
  
  A.rr <- A.trr <- A.rtr <- A.trtr <- vector('list', R)
  Z.rr <- Z.trr <- Z.rtr <- vector('list', R)
  
  for(i in 1:R){
    A.rr[[i]]   = A %*% diag(c(lq[[i]]))
    A.trr[[i]]  = A - A.rr[[i]]
    A.trtr[[i]] = A %*% diag(c(lqtilde[[i]]))
    A.rtr[[i]]  = A - A.trtr[[i]]
    
    Z.rr[[i]]  = A.rr[[i]] %*% diag(c(X[[i]]))
    Z.trr[[i]] = A.trr[[i]] %*% diag(c(X[[i]]))
    Z.rtr[[i]] = A.rtr[[i]] %*% diag(c(X.tilde[[i]]))
  }
  
  Z.big = matrix(0, ncol = R*s, nrow = R*s)
  
  for(i in 1:R){
    index = seq((i-1)*s+1,i*s)
    Z.big[index, index] = Z.rr[[i]]
    for(j in setdiff(1:R, i)){
      index2 = seq((j-1)*s+1,j*s)
      Z.big[index, index2] = (1/2)*(1/(R-1)) * (Z.rtr[[i]] + Z.trr[[j]])
    }
  }
  
  A.big = Z.big %*% diag(c(1/unlist(X)))
  
  ras(A.big, unlist(X), unlist(U), unlist(V))
}




