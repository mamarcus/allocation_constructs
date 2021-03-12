partCoeff_1 <- function(V, PSI,...){# The alternate approach
  library(magrittr) # enables piping function
  num_pc <- t(V) *t(PSI)
  denom_pc <- 
  rowSums(num_pc) %>%
  diag(.) %>%
  `colnames<-`(colnames(PSI)) %>%
  `rownames<-`(colnames(V))
  PHI <- solve(denom_pc) %*% num_pc
  PHI
  }