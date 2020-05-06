feedback.loop = function(io, agg.sectors = FALSE, agg.regions = FALSE){
  if(!"InputOutput" %in% class(io)) stop('io should be of "InputOutput" class. See ?as.inputoutput')
  
  
  # Dealing with aggregating sectors or regions
  if(agg.sectors == TRUE){
    sectors = unique(io$RS_label[,2])
    io = agg.sector(io, sectors, newname = 'all.sectors')
  }
  if(agg.regions == TRUE){
    regions = unique(io$RS_label[,1])
    io = agg.region(io, regions, newname = 'all.regions')
  }
  
  # Grabbing what we need
  Z = io$Z
  n = dim(io$Z)[1]
  
  # Initiating an output object
  fl = list('loops' = rep(list(NULL), n), 'value' = NULL)
  
  ###############################################################
  # Starting calculating the feedback loops and feedback values #
  ###############################################################
  
  # Taking shortcuts for 1x1 systems
  if(agg.sectors == TRUE & agg.regions == TRUE){
    fl$loops[[1]] = matrix(1)
    fl$value      = io$Z
  } else {
    # We need to set up the equality constraints of one entry per row (n) and one per column (n)
    # And the inequality constraints of being between 0 and 1 for the feedback loop elements (n^2)
    pb = txtProgressBar(0, max = n, style = 3)
    for(l in 1:n){
      setTxtProgressBar(pb, l)
      vZ = matrix(Z) # Vectorizing Z for optimization
      # Setting up 2n equality constraints
      # first the column constraint
      Ae = matrix(0, nrow = 2*n, ncol = n^2)
      for(i in 1:n){
        # First the column constraints
        j = ((i-1)*n+1):(n*i)
        Ae[i,j] = 1
        # Then the row constraints
        k = seq(0, n*(n-1), by = n)
        k = k + i
        Ae[n+i, k] = 1
      }
      # Now let's add the inequality constraints
      Ae = rbind(Ae, diag(n^2))
      # Setting up RHS
      rhs = rep(1, 2*n + n^2) # 2*n equality constraints, n^2 inequality constraints
      constraint = c(rep('==', 2*n), rep ('<=', n^2))
      
      # Now for the solution
      opt = lp('max', vZ, Ae, constraint, rhs)
      fl$loops[[l]] = matrix(opt$solution, ncol = n)
      fl$value = c(fl$value, opt$objval)
      
      # Setting up for the next loop
      Z = Z - 10^10 * fl$loops[[l]]
    }
  }
  fl
}