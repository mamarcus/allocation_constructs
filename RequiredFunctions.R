#Required  functions in global env
ndim <- function(x){length(dim(x))} #1 for vector,  2 for matrix , 3 and more for array
dimsize <- function(x){ prod(dim(x))} # number of elements e.g. 0 - vector , 1 or >1 - 2d or 3d matrix
mindim <- function(x){min(dim(x))}# dimension with lesser value for a 2d matrix
#array size
arraysize <- function(x, margin){ # for dimensions  greater than 2 espfor 3d and 4d arrays
  if (ndim(x) < 3) {warning ("Try nrows, ncols or length for matrices and vectors")}
  else if(ndim(x) == 3 && margin == 1) {dimsize(x)/prod(nrow(x),ncol(x))}
  else if(ndim(x) == 3 && margin == 2) {warning ("Array column dimensions unavailable")}
  else if(ndim(x) == 4 && margin == 1) { dimsize(x)/prod(nrow(x),ncol(x),dim(x)[ndim(x)])}
  else if(ndim(x) == 4 && margin == 2) { dimsize(x)/prod(nrow(x),ncol(x),dim(x)[ndim(x)-1])}
  else {warning ("Array dimensions unavailable")}
}
#robust diagnolization
ddiag <- function(x){ # robust diagnolization
  if(ndim(x) == 1){diag(x)}    # where x is a vector
  else if (ndim(x) == 2 && mindim(x) == 1) {diag(as.vector(x))} # where x is a matrix with a column or a row
  else if (ndim(x) == 2 && mindim(x) != 1){diag(diag(x))} # where x is a matrix with more than a column or a row
  else {warning("Input must be 1- or 2- dimensional")}
}