pa <- function(U, V, PSI, PHI=NULL, G=NULL,...){
#Take  SuUt or StUt Inventory, performs partition allocation and give intermediate matrix Z

# Input parameters
# U : Use table [com, ind] or [org, com, ind]
# V : Supply table [com, ind]
# PSI : Properties table [com, properties]
# PHI : Partition coefficient [ind, com](default=NULL)
# G : Unallocated emissions [ext, ind] (default= NULL)
  
# Returns
# Z : allocated intermediate flow matrix [com,ind,com] ¦ [org,com,ind,com]
# A : Normalized technical requirements (2-dimensional)
# nn_in : filter to remove np.empty rows in A or Z [com] | [org*com]
# nn_out : filter to remove np.empty columns in A or Z [ind*com]
# G_all : Allocated emissions [ext,ind,com]
# F : Normalized, allocated emissions [ext, com*ind]
 
   #Required dependencies
  library(magrittr)
  #partCoeff()
  #basic_variables()
  
  #default values
  G_all <- NULL
  F <- NULL
  
 #Basic variables
 basic_variables(U, V, G)  -> BV                            #gives an output of list which is BV[c("com", "ind", "org", "traceable","e_com","e_ind","q","g")]
 #BV[c("com", "ind", "org", "traceable")] 
 
 #calculating PHI
 if(is.null(PHI)) {PHI <- partCoeff(V, PSI)}
 
 # Partitioning of product flows
 if(BV["traceable"] == FALSE) {                            # for a SuUT, untraceable supply origin table
  Z <- array(0,c(BV$ind, BV$com, BV$com))                  #[matRows= BV$ind, matCols = BV$com,arrayRows= BV$com] (in python [array.rows, matrows, matcolumns])
      #Calculate Z 
      for (J in seq_along(1:BV$ind)) {
          Z[J,,] <- aperm(outer(U[,J],PHI[J,]))            #R, Fortran and Matlab uses Column-major order, therefore aperm(transpose) of array is performed
      }}
 else {                                                    # Where BV["traceable"] == TRUE && BV$org = nrow(U)
   Z <- array(0,c(BV$ind, BV$com, BV$org, BV$com))         #[matRows= BV$ind, matCols = BV$com,arrayRows= BV$org, arrayCols=BV$com] in python[arrayRows, arrayCols, matRows, matCols]
    for(I in seq_along(1:BV$org)){
      for(J in seq_along(1:BV$ind)){
        #eq: PAtrace
        Z[J, ,I,] <- aperm(outer(U[,J, I], PHI[J,]))       #Z[matrows=J, matcols, array.rows=I, array.cols]; U[matRows, matCols=J, arrayRows=I]; PHI[matRows=J, matCols]
      }
    } 
 }
  
 # matrixNormalizer (Z,V) -> (A, nn_in, nn_out)
 matrixNormalizer(Z, V) -> MN # holds a list MN$A, MN$nn_in, MN$nn_out
 
 #Allocation of environmental extensions
 if (dimsize(G)!= 0){
   G_all <- array(0, dim=c(ind, com,BV$ext))
   for (J in seq_along(1:ind)){
     G_all[J,,] <- aperm(outer(G[,J], PHI[J,]))
   }}
  
 #Normalize environmental extensions
   matrixNormalizer(G_all, V) -> MNe
   
  
 # Return results
   list(Z=Z, A=MN$A, nn_in = MN$nn_in, nn_out= MN$nn_out, G_all= G_all, F=MNe$A)
  }