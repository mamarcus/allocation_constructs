collapse_dimensions <- function(X,first2dim=FALSE,...){
  # For a 4d array, array rows and array columns are combined to a 3d array
  # Further, for the 3d array, the rows and columns are further combined to a 2d array
  # A No loop code is proposed in this method compared to python and matlab scripts
  # In R it is also possible to write a oneline code for this, see collapse_dimensions_1 
  
  #Input
  #X : 3d or 4d array to be collapsed
  #Output
  #X2d : Flatened 2d array
  
  #Required functions
  #ndim 
  #arraysize 
  
  # For a 4d array convert to 3d array...
    if (ndim(X) == 4){
      org <-arraysize(X, 1) #4 
      com <- arraysize(X, 2) #3
      ind <- nrow(X) #4
      X3d <- array(aperm(X, c(1,2,4,3)),dim=c(ind,com,org * com))
    }
  else{X3d <- X}
  
  if(ndim(X3d == 3) && first2dim == TRUE){
    org <- arraysize(X3d, 1)
    ind <- nrow(X3d) #  rows are  industries
    com <- ncol(X3d) # and columns are products
    X2d <- array(aperm(X3d, c(3,2,1)), dim=c(com, org*ind))
  }
  else if(ndim(X3d == 3)){
    org <- arraysize(X3d, 1)
    ind <- nrow(X3d)
    com <- ncol(X3d)
    X2d <- array(aperm(X3d, c(3,2,1)), dim=c(org, ind*com))
  }
  else if(ndim(X3d == 2)){ warning("Already 2 dimensional")}
  else{cat("Problem? ndim(Y) =", as.character(ndim(X3d)))}
  X2d
}
 
