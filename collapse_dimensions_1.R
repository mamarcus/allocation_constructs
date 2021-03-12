collapse_dimensions_1 <- function(X,first2dim=FALSE,...){
  #convert a 4d  into 2d in a one line code(minimal lines as possible)
  if (ndim(X) == 4 && first2dim == TRUE){
    org <-arraysize(X, 1) 
    com <- arraysize(X, 2) 
    ind <- nrow(X) 
    X2d <- aperm(array(aperm(X, c(1,2,4,3)), dim=c(com, prod(org, com, ind))),c(2,1))}
  else if(ndim(X)==4){
    org <-arraysize(X, 1)  
    com <- arraysize(X, 2) 
    ind <- nrow(X) 
    X2d <- aperm(array(aperm(X, c(1,2,4,3)), dim=c(org*com, ind*com)),c(2,1))
  }
  else {warning("This matrix is not a 4d matrix")}
    X2d
}