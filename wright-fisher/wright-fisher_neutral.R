#==============================
# Wright-Fisher model (neutral)
#==============================

num <- seq( 0, 9 ) # initial population 
num_size <- length(num) # sum of population 
print( paste( "0:", paste(num, collapse = ",") ) )

# simulation 
geneNum <- 10 
for ( t in 1:geneNum ) {
  numAfter <- sample( num, size=num_size, replace=TRUE ) # random extraction from a vector
  print( paste( "t:", paste(numAfter, collapse = ",") ) )
  num <- numAfter # updating a vector 
}
