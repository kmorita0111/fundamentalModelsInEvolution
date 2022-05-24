calc = function( surroundings, qk ){
  
  #print( surroundings )
  #print( sum( surroundings ) )
  
  if( sum( surroundings ) == 0 ){
    #print( "0!" ); print( "===" )
    return( sample( c(0,1), 1, prob=c((1-qk[1]),qk[1]) ) )
    
  } else if( sum( surroundings ) == 1 ){
    #print( "1!" ); print( "===" )
    return( sample( c(0,1), 1, prob=c((1-qk[2]),qk[2]) ) )
    
  } else if( sum( surroundings ) == 2 ){
    #print( "2!" ); print( "===" )
    return( sample( c(0,1), 1, prob=c((1-qk[3]),qk[3]) ) )
    
  } else if( sum( surroundings ) == 3 ){
    #print( "3!" ); print( "===" )
    return( sample( c(0,1), 1, prob=c((1-qk[4]),qk[4]) ) )
    
  } else if( sum( surroundings ) == 4 ){
    #print( "4!" ); print( "===" )
    return( sample( c(0,1), 1, prob=c((1-qk[5]),qk[5]) ) )
  }
  
}
