feedback.loop.matrix = function(fl, loop){
  if(class(fl) != 'FeedbackLoop'){stop('fl must be a "FeedbackLoop object. See ?feedback.loop')}
  if(missing(loop)){stop('Which loop do you want? Specify loop = ')}
  
  # Initiating the matrix
  n = length(fl$value)
  if(loop > n){stop(cat('There are fewer than ', loop, 
                        ' loops. Choose a number weakly less than ', n, '.', sep = ''))}
  fl.mat = matrix(0, nrow = n, ncol = n)
  
  for(i in 1:n){
    fl.mat[i, fl$loops[[loop]][i,2] ] = 1
  }
  fl.mat
}
