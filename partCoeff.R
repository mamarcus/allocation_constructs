partCoeff <- function(V, PSI,...){
  # Equivalent functions between python and r:http://mathesaurus.sourceforge.net/r-numpy.html
  # Input parameters
  # V : Supply table [com, ind]
  # PSI : Properties table[com, properties]
  # ... : equivalent to vargin of matlab
  
  #Output parameters
  #PHI : Partition coefficients[ind, com]. 
  # -partitioned by intensive properties(mass, energy density, price, etc.)
  
  #Required functions
  ndim <- function(x){length(dim(x))} # number of dimensions 
  mindim <- function(x){min(dim(x))}# dimension with lesser value for a 2d matrix
  ddiag <- function(x){ # robust diagnolization
    if(ndim(x) ==0){diag(x)}    # where x is a vector
    else if (ndim(x) == 2 && mindim(x) == 1) {diag(as.vector(x))} # where x is a matrix with a column or a row
    else if (ndim(x) == 2 && mindim(x) != 1){diag(diag(x))} # where x is a matrix with more than a column or a row
    else {warning("Input must be 1- or 2- dimensional")}
  }
  
  #----------Step 1
  library(magrittr) # enables piping function
  # denominator: diag(diag(t(V) %*% PSI)
  denom <-           # denominator gives the total industry output( w.r.t mass, energy)of all commodities. It is row sum of t(V) * t(PSI)
    t(V) %*% PSI %>% 
    ddiag(.)%>% # diag(diag()) would be suffice since any change in dimensions will affect num
    `colnames<-`(colnames(PSI)) %>%
    `rownames<-`(colnames(V))
  
  #----------Step 2
  num <- t(V) * t(PSI) #Numerator: disaggregated share of commodities outputfrom each industry based on partition coeff. (PSI)
  
  #----------Step 3
  PHI <- solve(denom) %*% num # partition coefficients per unit output of industry   
  PHI
  }

  