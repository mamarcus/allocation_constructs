basic_variables <- function(U, V, G,...){ # calculate intermediate variables from SUTs and emission tables 
  
  #Input variables
  #U: Use table[com, ind]
  #S: Supply table[com, ind]
  #G : Unallocated emissions [ext, ind] (default= 0)
  
  #output 
  #com : number of commodities (products)
  #ind : number of industries (activities)
  #org : number of origin industries (for traceable flows)
  #traceable : boolean, are use flows traceable, true or false?
  #e_com : vertical vector of ones [com, 1]
  #e_ind : vertical vector of ones [ind, 1]
  #q : total production volume of each commodity
  #g : total production volume of each industry
  #ext : number of environmental stressors/factors of production
  
  #Required functions
  ndim <- function(x){length(dim(x))} #1 for vector,  2 for matrix , 3 and more for array
  dimsize <- function(x){ prod(dim(x))} # number of elements e.g. 0 - vector , 1 or >1 - 2d or 3d matrix
  #dim(x)  give the dimension of a matrix equivalent to shape in python
  #ncol() and nrow()  always gives the no of rows and no of columns in a matrix
  arraysize <- function(x, margin){ # for dimensions  greater than 2 espfor 3d and 4d arrays
    if (ndim(x) < 3) {warning ("Try nrows, ncols or length for matrices and vectors")}
    else if(ndim(x) == 3 && margin == 1) {dimsize(x)/prod(nrow(x),ncol(x))}
    else if(ndim(x) == 3 && margin == 2) {warning ("Array column dimensions unavailable")}
    else if(ndim(x) == 4 && margin == 1) { dimsize(x)/prod(nrow(x),ncol(x),dim(x)[ndim(x)])}
    else if(ndim(x) == 4 && margin == 2) { dimsize(x)/prod(nrow(x),ncol(x),dim(x)[ndim(x)-1])}
    else {warning ("Array dimensions unavailable")}
  }
  
  #Step1 : Traceability and dimension
  com <- nrow(V) # number of rows of supply table
  ind <- ncol(V)
  if (ndim(U) == ndim(V)){ # Untraceable
    traceable <- FALSE
    org <- 1}
  else if (ndim(U) == ndim(V)+1){ #Traceable
    traceable <- TRUE
    org <- arraysize(U,1)}
  else {warning("Incompatible dimensions")}

  #step 2: Assiging the environmental externalities
  ext <- vector(mode="numeric", length=0) #  can also be ext <- NULL
  if(dimsize(G)!= 0) {ext = nrow(G)}

  #step 3 - summation vectors
  e_com <- array(1, com)
  e_ind <- array(1, ind)
  
  #step4 - Totals
  #industry total
  g <- t(V) %*% e_com # alternate would be g <- rowSums(t(V))
  q <- V %*% e_ind    # alternate would be q <- rowSums(V) 
  
  #Step 5
  BV <-  list(com=com, ind=ind, org=org, traceable=traceable, e_com=e_com, e_ind=e_ind, q=q, g=g, ext=ext)
  BV
}
  
