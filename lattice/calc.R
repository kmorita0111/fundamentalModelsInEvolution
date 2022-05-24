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
