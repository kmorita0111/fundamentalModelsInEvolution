#=========================
# canopy model
#=========================

###=== functions ===###
calc = function( surroundings, qk ){
  if( sum( surroundings ) == 0 ){
    return( sample( c(0,1), 1, prob=c(1-qk[1],qk[1]) ) )
  } else if( sum( surroundings ) == 1 ){
    return( sample( c(0,1), 1, prob=c(1-qk[2],qk[2]) ) )
  } else if( sum( surroundings ) == 2 ){
    return( sample( c(0,1), 1, prob=c(1-qk[3],qk[3]) ) )
  } else if( sum( surroundings ) == 3 ){
    return( sample( c(0,1), 1, prob=c(1-qk[4],qk[4]) ) )
  } else if( sum( surroundings ) == 4 ){
    return( sample( c(0,1), 1, prob=c(1-qk[5],qk[5]) ) )
  }
}
###===
updateMode <- 0 # 0; Sequentially / 1; simultaneous
parePath <- "~/fundamentalModelsInEvolution/lattice"

###=== setting an initial lattice ===###
Nl <- 10 # size of lattice
lat <- matrix(0, Nl, Nl) # lattice 
for ( i in 1:Nl ) {
  lat[i,] <-  sample( c(0,1), Nl, replace=T )
}

png( paste( paste( parePath, paste("patch", formatC(t,width=4,flag="0"), sep="_"), sep="/result/" ), "png", sep="."), 
     width=6, height=5, units = "in", res=300 
  )
par(mar=c(3, 3, 3, 3))

image( c(1:Nl), c(1:Nl), lat, 
       xlim=c(1,Nl), ylim=c(1,Nl), col=terrain.colors(2), 
       xlab="X", ylab="Y", main=paste("Generation t =", formatC(0,width=4,flag="0") ) 
      )
#par(ps=15)
#for ( i in 1:Nl ) {
#  for ( j in 1:Nl ) {
#    text( i-0.5, j-0.5, as.character( lat[i,j] ),
#          col="black", font=2
#    )
#  }
#}
dev.off()

# qk; a central individual becomes 1 when surrounded by 1's k individuals
survival_prob <- function(x){ 1 - x/4 } 
q0 <- survival_prob(0); q1 <- survival_prob(1); q2 <- survival_prob(2) 
q3 <- survival_prob(3); q4 <- survival_prob(4)
qk <- c( q0, q1, q2, q3, q4 )

if( updateMode == 0 ){
  
  gener <- 10
  for ( t in 1:gener ) {
   
    i <- sample( seq(1,Nl), 1 )
    j <- sample( seq(1,Nl), 1 )
    
    if( i == 1 ){
      if( j == 1 ){
        lat[1,1] <- calc( c(lat[Nl,1], lat[2,1], lat[1,Nl], lat[1,2]), qk ) # 上下左右
      } else if( j == Nl ){
        lat[1,Nl] <- calc( c(lat[Nl,1], lat[2,1], lat[1,Nl-1], lat[1,1]), qk )
      } else {
        lat[i,j] <- calc( c(lat[Nl,j], lat[2,j], lat[1,j-1], lat[1,j+1]), qk )
      } 
    } else if( i == Nl ){
      if( j == 1 ){
        lat[Nl,1] <- calc( c(lat[Nl-1,1], lat[1,1], lat[Nl,Nl], lat[Nl,2]), qk ) # 上下左右
      } else if( j == Nl ){
        lat[Nl,1] <- calc( c(lat[Nl-1,Nl], lat[1,Nl], lat[Nl,Nl-1], lat[Nl,1]), qk )
      } else {
        lat[i,j] <- calc( c(lat[Nl-1,j], lat[1,j], lat[Nl,j-1], lat[Nl,j+1]), qk )
      } 
    } else {
      if( j == 1 ){
        lat[Nl,1] <- calc( c(lat[i-1,1], lat[i+1,1], lat[i,Nl], lat[i,2]), qk ) # 上下左右
      } else if( j == Nl ){
        lat[Nl,1] <- calc( c(lat[i-1,Nl], lat[i+1,Nl], lat[i,Nl-1], lat[i,1]), qk )
      } else {
        lat[i,j] <- calc( c(lat[i-1,j], lat[i+1,j], lat[i,j-1], lat[i,j+1]), qk )
      } 
    }
    
    ###--- graphics ---###
    png( paste( paste( parePath, paste("patch", formatC(t,width=4,flag="0"), sep="_"), sep="/result/" ), "png", sep="."), 
         width=6, height=5, units = "in", res=300 
        )
    par(mar=c(3, 3, 3, 3))
    
    image( c(1:Nl), c(1:Nl), lat, 
           xlim=c(1,Nl), ylim=c(1,Nl), col=terrain.colors(2), 
           xlab="X", ylab="Y", main=paste("Generation t =", formatC(t,width=4,flag="0") ) 
          )
    
    dev.off()
    
  }
  
} else if( updateMode == 1 ){

  for ( i in 1:Nl ) {
    for ( j in 1:Nl ) {
      
      # upper 
      if( i == 1 ){
        if( j == 1 ){
          lat[1,1] <- calc( c(lat[Nl,1], lat[2,1], lat[1,Nl], lat[1,2]), qk )
        } else if( j == Nl ){
          lat[i,j]
        } else {
          lat[i,j]
        } 
      } else if( i == Nl ){
        
      } else if( i == Nl ){
        
      }
      
    }
  }
  
}
