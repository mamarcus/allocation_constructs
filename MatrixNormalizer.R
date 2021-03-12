matrixNormalizer <- function(Z, V,...){
  # normalizes a flow matrix when some rows and columns are null
  
  # Input parameters
  # ----------
  # Z : Flow matrix to be normalized dimensions 
  #     : [com, com] or  [ind,com,com] | [ind,com,ind,com]
  # V : Production volume with which flows are normalized [com, ind]
  # 
  # Output
  # --------
  # A : Normalized flow matrix, without null rows and columns
  # nn_in : filter applied to rows (0 for removed rows, 1 for kept rows)
  # nn_out : filter applied to cols (0 for removed cols, 1 for kept cols)
  
  #dependent packages
  library(Matrix)
  
  # Collapse dimensions
  if(ndim(Z)>2){Z <- collapse_dimensions(Z)}
    
  # Basic variables estimation
    com <- nrow(V)
    ind <- ncol(V)
    comZ <- nrow(Z)
  
  # Total production, both aggregate and traceable
    q <- colSums(V) # total supply
    u <- colSums(Z) # total use
    q_tr <- array(V, dim=ind*com) # R naturally orders the 2-D matrix of q_tr into a column ordered vector
  
  #Filter inputs to preserve commodities to get a matrix square or recipe right!
  #nn_in
  if(nrow(Z) == com) {nn_in <-  (abs(q) +abs(u)) != 0 } # resultant vector with elements of logical vector
  else if (nrow(Z) == com*ind){nn_in <- (abs(q_tr)+abs(u)) != 0 }
  else {nn_in <- array(TRUE, comZ)}

  #nn_out
  if(ncol(Z) == com){
    nn_out <- q != 0
    A <- Z[nn_in, ][,nn_out] %*% solve(ddiag(q[nn_out]))}
  else if(ncol(Z) == (com*ind)){
    nn_out <- q_tr != 0
    A <- Z[nn_in, ][,nn_out] %*% solve(ddiag(q_tr[nn_out]))}
  else { 
    nn_out <- array(TRUE, ncol(Z))
    A <- Z %*% solve(ddiag(q_tr))}
  
  # Return
   list(A=A, nn_in= nn_in, nn_out= nn_out) 
  }
