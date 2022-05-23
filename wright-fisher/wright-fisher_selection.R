#=================================
# Wright-Fisher model (selection)
#=================================
###--- loading library ---###
library(stringr)

###--- initial values ---###
N0 <- 1; N1 <- 1; Nall <- N0 + N1 
num0 <- c( rep(0,N0), rep(1,N1) )  # initial population 

prob_weight <- rep( 1/Nall,Nall ) # weight of probability 

r <- 2   # selection pressure; r = 1 -> neutral / r > 1 -> selection 
num_0fixed <- 0; num_1fixed <- 0 # counting time of fixation of each allele

###--- simulation ---###
geneNum <- 100
all_trial <- 10000
for ( num_trial in 1:all_trial ) {
  
  num <- num0
  #print( paste( as.character(str_pad(0, 3, pad=0)) , paste(num, collapse = ","), sep=" : "  ) )
  
  for ( t in 1:geneNum ) {
    
    if( length( which( num>0 ) ) > 0 && length( which( num>0 ) ) < Nall ){ # allele 1 exists
      N1 <- length( which( num>0 ) )
      prob_weight[-which( num>0 )] <- 1/( Nall - (1-r)*N1 )
      prob_weight[which( num>0 )] <- r/( Nall - (1-r)*N1 )
    } else if( length( which( num>0 ) ) <= 0 ){ # no allele 1 
      prob_weight[1:Nall] <- 1/Nall
    } else if( length( which( num>0 ) ) >= Nall ){ # no allele 1 
      prob_weight[1:Nall] <- 1/Nall
    } 
    #print( prob_weight )
    numAfter <- sample( num, size=Nall, replace=TRUE, prob=prob_weight ) # for ( i in 1:num_size ) { numAfter[i] <- sample( num, size=1, prob=prob_weight ) }
    num <- numAfter
    #print( paste( as.character(str_pad(t, 3, pad=0)) , paste(numAfter, collapse = ","), sep=" : "  ) )
    
    if( sum( num ) == 0 ){
      num_0fixed <- num_0fixed + 1
      #print( paste( "0 is fixed: t =", as.character(str_pad(t, 3, pad=0)) ) ) 
      break
    }  else if( sum( num ) == Nall ){
      num_1fixed <- num_1fixed + 1
      #print( paste( "1 is fixed: t =", as.character(str_pad(t, 3, pad=0)) ) ) 
      break
    } 
    
  }
  #print("===")
}

# calculating fixation probability
print( paste( "fixation probability =", as.character(num_1fixed/all_trial) ) )
