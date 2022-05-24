#=========================
# canopy model
#=========================

parePath <- "~/fundamentalModelsInEvolution/lattice"

###=== setting an initial lattice ===###
Nl <- 10 # size of lattice
lat <- matrix(0, Nl, Nl) # lattice 
afterLat <- matrix(0, Nl, Nl) # lattice 
set.seed(1) # setting seed
for ( i in 1:Nl ) {
  lat[i,] <-  sample( c(0,1), Nl, replace=T )
}

png( paste( paste( parePath, paste("patch", formatC(0,width=4,flag="0"), sep="_"), sep="/result/" ), "png", sep="."), 
     width=6, height=5, units = "in", res=300 
  )
par(mar=c(3, 3, 3, 3))

image( c(1:Nl), c(1:Nl), (1-lat), 
       # xlim=c(1,Nl), ylim=c(1,Nl), 
       axes = FALSE, 
       col=terrain.colors(2), 
       xlab="X", ylab="Y", main=paste("Generation t =", formatC(0,width=4,flag="0") ) 
      )
axis(1, at = seq(1, Nl, by = 1))
axis(2, at = seq(1, Nl, by = 1))
box()
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
survival_prob01 <- function(x){ 1 - 0.5*x/4 } 
survival_prob02 <- function(x){ 0.8*exp(-(x^2)/4) + ( 1 - 0.8*exp(0) ) } 
survival_prob03 <- function(x){ 0.8*( -(x-4)/( 1-(x-4) ) ) } 
q0 <- survival_prob01(0); q1 <- survival_prob01(1); q2 <- survival_prob01(2); q3 <- survival_prob01(3); q4 <- survival_prob01(4); print( "type 1 (linear) ..." )
#q0 <- survival_prob02(0); q1 <- survival_prob02(1); q2 <- survival_prob02(2); q3 <- survival_prob02(3); q4 <- survival_prob02(4); print( "type 2 (gaussian) ..." )
#q0 <- survival_prob03(0); q1 <- survival_prob03(1); q2 <- survival_prob03(2); q3 <- survival_prob03(3); q4 <- survival_prob03(4); print( "type 3 (disk) ..." )
qk <- c( q0, q1, q2, q3, q4 )

###=== functions ===###
source( paste( parePath, "calc.R", sep="/" ) )

updateMode <- 0 # 0; Sequentially / 1; simultaneous

###=== simulation ===###
if( updateMode == 0 ){
  
  gener <- 1000
  for ( t in 1:gener ) {
   
    i <- sample( seq(1,Nl), 1 )
    j <- sample( seq(1,Nl), 1 )
    
    if( i == 1 ){
      if( j == 1 ){
        lat[1,1] <- calc( c(lat[Nl,1], lat[2,1], lat[1,Nl], lat[1,2]), qk ) # 上下左右
      } else if( j == Nl ){
        lat[1,Nl] <- calc( c(lat[Nl,Nl], lat[2,Nl], lat[1,Nl-1], lat[1,1]), qk )
      } else {
        lat[1,j] <- calc( c(lat[Nl,j], lat[2,j], lat[1,j-1], lat[1,j+1]), qk )
      } 
    } else if( i == Nl ){
      if( j == 1 ){
        lat[Nl,1] <- calc( c(lat[Nl-1,1], lat[1,1], lat[Nl,Nl], lat[Nl,2]), qk ) # 上下左右
      } else if( j == Nl ){
        lat[Nl,Nl] <- calc( c(lat[Nl-1,Nl], lat[1,Nl], lat[Nl,Nl-1], lat[Nl,1]), qk )
      } else {
        lat[Nl,j] <- calc( c(lat[Nl-1,j], lat[1,j], lat[Nl,j-1], lat[Nl,j+1]), qk )
      } 
    } else {
      if( j == 1 ){
        lat[i,1] <- calc( c(lat[i-1,1], lat[i+1,1], lat[i,Nl], lat[i,2]), qk ) # 上下左右
      } else if( j == Nl ){
        lat[i,Nl] <- calc( c(lat[i-1,Nl], lat[i+1,Nl], lat[i,Nl-1], lat[i,1]), qk )
      } else {
        lat[i,j] <- calc( c(lat[i-1,j], lat[i+1,j], lat[i,j-1], lat[i,j+1]), qk )
      } 
    }
    
    ###--- graphics ---###
    png( paste( paste( parePath, paste("patch", formatC(t,width=4,flag="0"), sep="_"), sep="/result/" ), "png", sep="."), 
         width=6, height=5, units = "in", res=300 
        )
    par(mar=c(3, 3, 3, 3))
    
    image( c(1:Nl), c(1:Nl), (1-lat), # inversion of matrix due to color palette
           #xlim=c(1,Nl), ylim=c(1,Nl), 
           axes = FALSE, 
           col=terrain.colors(2), 
           xlab="X", ylab="Y", main=paste("Generation t =", formatC(t,width=4,flag="0") ) 
          )
    axis(1, at = seq(1, Nl, by = 1))
    axis(2, at = seq(1, Nl, by = 1))
    box()
    
    xNum <- as.character(i); yNum <- as.character(j)
    pnt <- paste( "(", ")", sep=paste( xNum, yNum, sep="," ) )
    text( i, j, pnt, #as.character( lat[i,j] ),
          col="black", font=2
        )
    
    dev.off()
    
  }
  
} else if( updateMode == 1 ){

  for ( i in 1:Nl ) {
    for ( j in 1:Nl ) {
      
      if( i == 1 ){
        if( j == 1 ){
          afterLat[1,1] <- calc( c(lat[Nl,1], lat[2,1], lat[1,Nl], lat[1,2]), qk ) # 上下左右
        } else if( j == Nl ){
          afterLat[1,Nl] <- calc( c(lat[Nl,Nl], lat[2,Nl], lat[1,Nl-1], lat[1,1]), qk )
        } else {
          afterLat[1,j] <- calc( c(lat[Nl,j], lat[2,j], lat[1,j-1], lat[1,j+1]), qk )
        } 
      } else if( i == Nl ){
        if( j == 1 ){
          afterLat[Nl,1] <- calc( c(lat[Nl-1,1], lat[1,1], lat[Nl,Nl], lat[Nl,2]), qk ) # 上下左右
        } else if( j == Nl ){
          afterLat[Nl,Nl] <- calc( c(lat[Nl-1,Nl], lat[1,Nl], lat[Nl,Nl-1], lat[Nl,1]), qk )
        } else {
          afterLat[Nl,j] <- calc( c(lat[Nl-1,j], lat[1,j], lat[Nl,j-1], lat[Nl,j+1]), qk )
        } 
      } else {
        if( j == 1 ){
          afterLat[i,1] <- calc( c(lat[i-1,1], lat[i+1,1], lat[i,Nl], lat[i,2]), qk ) # 上下左右
        } else if( j == Nl ){
          afterLat[i,Nl] <- calc( c(lat[i-1,Nl], lat[i+1,Nl], lat[i,Nl-1], lat[i,1]), qk )
        } else {
          afterLat[i,j] <- calc( c(lat[i-1,j], lat[i+1,j], lat[i,j-1], lat[i,j+1]), qk )
        } 
      }
      
    }
  }
  
  ###--- graphics ---###
  png( paste( paste( parePath, paste("patch", formatC(t,width=4,flag="0"), sep="_"), sep="/result/" ), "png", sep="."), 
       width=6, height=5, units = "in", res=300 
      )
  par(mar=c(3, 3, 3, 3))
  
  image( c(1:Nl), c(1:Nl), (1-afterLat), 
         #xlim=c(1,Nl), ylim=c(1,Nl), 
         axes = FALSE, 
         col=terrain.colors(2), 
         xlab="X", ylab="Y", main=paste("Generation t =", formatC(t,width=4,flag="0") ) 
        )
  axis(1, at = seq(1, Nl, by = 1))
  axis(2, at = seq(1, Nl, by = 1))
  box()
  
  xNum <- as.character(i); yNum <- as.character(j)
  pnt <- paste( "(", ")", sep=paste( xNum, yNum, sep="," ) )
  text( i, j, pnt, #as.character( lat[i,j] ),
        col="black", font=2
      )
  
  dev.off()
  
  ###--- updating ---###
  lat <- afterLat
  
}
print( "simulation completed!" )
