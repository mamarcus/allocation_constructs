arraysize <- function(x, margin){ # for dimensions  greater than 2 espfor 3d and 4d arrays
  ndim <- function(x){length(dim(x))}
  dimsize <- function(x){ prod(dim(x))}
  if (ndim(x) < 3) {warning ("Try nrows, ncols or length for matrices and vectors")}
  else if(ndim(x) == 3 && margin == 1) {dimsize(x)/prod(nrow(x),ncol(x))}
  else if(ndim(x) == 3 && margin == 2) {warning ("Array column dimensions not available")}
  else if(ndim(x) == 4 && margin == 1) { dimsize(x)/prod(nrow(x),ncol(x),dim(x)[ndim(x)])}
  else if(ndim(x) == 4 && margin == 2) { dimsize(x)/prod(nrow(x),ncol(x),dim(x)[ndim(x)-1])}
  else {warning ("Array dimensions unavailable")}
}