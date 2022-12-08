M_input <- M_all
 b_1  <- 1
r_1 <- 1800
   
   
  lsh4 <- function(M_input, b_1 , r_1){
   
  col_length_M_imput <-  1624
  i <- 0
  bands <- 0
  
  duplicates_1 <- matrix(0,col_length_M_imput,col_length_M_imput)
  
  #op 2 moet b staan 
  for(bands_1 in 0 : (b_1 - 1) ){
    print(paste(bands_1 , "of", b_1 ))
    band_matrix_1 <- matrix(0, r_1, 1624)
    
    band_index_1_1 <- (1 + bands_1*r_1) 
    band_index_1_2 <- (r_1 + bands_1*r_1)
    if(bands == (b_1)){
      # band_matrix_1 <- M_all[ c(( band_index_1_1 -1) : (dim(M_all)[1]) ) , c(1 : 2222 )]
      print(paste("je moeder"))
    }
    else{
      band_matrix_1 <- M_input[ c( band_index_1_1 : band_index_1_2 ) , c(1 : 1624 )]
    }
    #print(band_index_1)
    ant_col <- 1624
    for(i in 1: (ant_col -1)){
      
      for(j in (i+1) : (ant_col -1) ){
        
        if(!(i >1624)){
        if(identical( band_matrix_1[,i] , band_matrix_1[,j] ) ){
          duplicates_1[i, j ] <- 1
        }
        }
      }
    }
    
  }
 return(duplicates_1)
}