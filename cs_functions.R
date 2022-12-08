

import_data_1 = function(path){
  
 # path <- "~/Documents/uni/master/cs/TVs-all-merged.json"
  json_file <- fromJSON(file = path)
  data_1 <- c()
  i <-0
  z <- 1
  dummy_new <-list()
  test2 <- c(2000,2000)
  # goes over all product
  for(x in json_file){
    #goes over the dubbels
    counter <- 1
    i<- i+1
    for(t in 1 : length(x)){
      test2 <- json_file[[names(json_file)[i]]][[t]][["title"]]
      string_dummy <- ""
      for(k in 1:length(test2)){
        string_dummy <- paste( string_dummy ,names(test2)[k]  , test2[k] )     
      }
      dummy_new[z] <- string_dummy
      z <- z+1
      counter <- counter +1
    }
  }
  return(dummy_new)
}
#extracs the model id s 
import_data_2 = function(path){


#alles
#hier onder is goed

path <- "~/Documents/uni/master/cs/TVs-all-merged.json"
json_file <- fromJSON(file = path)
#dummy variables 
data_1 <- c()
i <-0
z <- 1
#set to zero for all the data 
thinginvar <- 0

dummy_new <-list()
model_id_new <-list()
test2 <- c(2000,2000)
test3 <- c(2000,2000)
# goes over all product
for(x in json_file){
  #goes over the dubbels
  counter <- 1
  i<- i+1
  for(t in 1 : length(x)){
    test2 <- json_file[[names(json_file)[i]]][[t]][["title"]]
    test3 <- json_file[[names(json_file)[i]]][[t]][["modelID"]]
    
    string_dummy <- ""
    model_id_dummy <- ""
    for(k in 1:length(test2)){
      model_id_dummy <- paste( model_id_dummy ,names(test3)[k]  , test3[k] )   
      string_dummy <- paste( string_dummy ,names(test2)[k]  , test2[k] )     
    }
    model_id_new[z] <- model_id_dummy
    dummy_new[z] <- string_dummy
    z <- z+1
    counter <- counter +1
  }
}
return(model_id_new )

}

bootstrap <- function(){
  
  
  
  
  
  
  
  return()
  
}

Calculate_r_t <-function(b,n){
  
  r<-c()
  t<-c()
  
  for(i in 1 : length(b) ){
  
  r[i] <- floor( (n/b[i]) )
  t[i] <- (1/b[i])^(1/r[i])
  }
  return(cbind(r,t))
}

boot_strap = function(dummy_new , model_id_new){
  
  #makes boot strp 
  c<- c(1:1624)
  control <- c(1:1624)
  boot <- sample(c , 1624, replace = T)
  dummy_new_boot <- list()
  out_of_sample_titel <- list()
  out_of_sample_id <- list()
  dummy_new_boot_model_id <-list()
  #make the bootstrap sample in sample
  for(i in 1: length(dummy_new)){
    #the model id for the boot strap 
    index_5 <- boot[i]
    dummy_new_boot_model_id[i] <- model_id_new[index_5]
    #this gives the titel corresponing to the index
    index_3 <- boot[i]
    dummy_new_boot[i] <- dummy_new[index_3]
    #find the out of sample
    control <- control[-c(index_3)]
  }
  # make the out of sample
  i <- 0
  for(i in 1: length(control)){
    index_4 <- control[i]
    out_of_sample_titel[i] <- dummy_new[index_4]
    out_of_sample_id[i] <- model_id_new[index_4]
  }
  #neibor matrix for the out of sammple
  
  neibormatrix_out <- matrix(0,length(out_of_sample_id) , length(out_of_sample_id) )
  for(r in 1: length(out_of_sample_id)){
    
    for(c in r: length(out_of_sample_id) ){
      if(identical(out_of_sample_id[[r]], out_of_sample_id[[c]])){
        
        neibormatrix_out[r,c] <- 1
        #neibormatrix_out[c,r] <- 1
        #forces that a product is not a duplicut of it self
        neibormatrix_out[r,r] <- 0
      }
    }
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}

true_nehbor_matrix <- function(  dummy_new_boot_model_id  ){
  
  neibormatrix_in <- matrix(0,length(dummy_new_boot_model_id) , length(dummy_new_boot_model_id) )
  for(r in 1: length(dummy_new_boot)){
    
    for(c in r: length(dummy_new_boot) ){
      #hier gaat iets fout
      if(identical(dummy_new_boot_model_id[[r]], dummy_new_boot_model_id[[c]])){
        neibormatrix_in[r,c] <- 1
        neibormatrix_in[r,r] <- 0
      }
    }
  }
  return(neibormatrix_in)
}

extract_ids = function(dummy_new_8){
  model_ids <- c()
  i <- 0
  for (i in 1:length(dummy_new_8)) {
    # title = dummy_new[[i]]
    # words_list =  str_split(title, " ")
    words_in_array =  str_split(dummy_new_8[[i]] , " ")
    boolean = grepl("[a-zA-Z0-9]*(([0-9]+[^0-9, ]+)|([^0-9, ]+[0-9]+))[a-zA-Z0-9]*", words_in_array[[1]],  perl = F) 
    for(j in 1:length(boolean)){
      if(boolean[j] == F){
      }
      bolean_indicator <- boolean[j] &&( nchar(words_in_array[[1]][j]) > 3) && !grepl("hz|Hz|1080p|720p|2012|Inch|2011|2010|inch|120HZ|240HZ",  words_in_array[[1]][j])
      if( bolean_indicator ){ 
        model_ids[i] =  words_in_array[[1]][j]
      }
    }
  }
  if(length(model_ids) < 1624){
    for(k in 1 : (1624 -length(model_ids))){
      model_ids <- c(model_ids, NA)
    }
  }
  return(model_ids)
}

estimation_model_id <- function(model_ids){
  
  l <- length(model_ids)
  
  est_with_model_id <- matrix(0 ,l,l)
  
  
  for(r in 1 :l ){
    for(c in r:l){
       if(identical(model_ids[r], model_ids[c])   && !is.na(model_ids[r]) && !is.na(model_ids[c])) {
         est_with_model_id[r,c] <- 1
         est_with_model_id[r,r] <- 0
      }
    }
  }
  return(est_with_model_id)
}

space_one = function(K,bin_not_matrix2){
  for(i in 1:length(K)){
    index = K[i]
    if(bin_not_matrix2[index]==1){
      return(i)
    }
  }
}
#remove spaces
dicksonary <- function(dummy_new , thinginvar){
  test45 <- matrix(0,)
  o <- list()
  #shingels sets 
  dicsonary <- vector()
  for(g in 1:( length(dummy_new) - thinginvar) ){
    
    for(q in 1: (nchar(dummy_new[[g]]) - 10 )){
      dummy_string <- substr(dummy_new[[g]] , q, q+2)
      
      if( !any(dummy_string == dicsonary) ){
        dicsonary <- c(dicsonary, dummy_string)
      }
    }
  }
  
  
  return(dicsonary)
  
}

binary_matrix1 <- function(dummy_new, thinginvar){
  
  binary_matrix<- matrix(0, length(dicsonary) , length(dummy_new) - thinginvar)
  #now make the binary matrix
  for(p in 1:( length(dummy_new) - thinginvar) ){
    
    for(q in 1: (nchar(dummy_new[[p]]) - 2 )){
      dummy_string <- substr(dummy_new[[p]] , q, q+2)
      
      if( any(dummy_string == dicsonary) ){
        row_n <- match(dummy_string , dicsonary)
        binary_matrix[row_n, p ] <- 1
      }
    }
  } 
  
  return(binary_matrix)
}

this_binary_matrix <- function(dummy_new4, this_dicsonary,thinginvar){
  
  this_binary_matrix<- matrix(0, length(this_dicsonary) , length(dummy_new4) - thinginvar)
  #now make the binary matrix
  for(p in 1:( length(dummy_new4) - thinginvar) ){
    
    for(q in 1: (nchar(dummy_new4[[p]]) - 2 )){
      dummy_string <- substr(dummy_new4[[p]] , q, q+2)
      
      if( any(dummy_string == this_dicsonary) ){
        row_n <- match(dummy_string , this_dicsonary)
        this_binary_matrix[row_n, p ] <- 1
      }
    }
  } 
  
  return(this_binary_matrix)
}

min_hashing <- function( binary_matrix,dicsonary ){
  
  #Number_indiecies <- round(length(dicsonary)/2 , digits = 0 )  
  Number_indiecies<-1800
  M <- matrix(0,Number_indiecies , dim(binary_matrix)[2] )
  # create the vectors with randomzed indicies
  #M is the signature matrix
  for(k in 1:Number_indiecies ){
    number_colum <- sample(seq(1:length(binary_matrix[,1])))
    
    if( (k/100)%%1 == 0 ){print(k)}
    
    for(i in 1:dim(binary_matrix)[2]){
      check <- 0
      counter_2 <- 0
      
      while(check == 0){
        counter_2 <- counter_2 +1
        position <- match(counter_2 , number_colum )
        check <- binary_matrix[position , i]
        if(check == 1){
          M[k , i] <- counter_2
        }
      }
    }
  }
  return(M)
}

lsh <- function(M , b){
  
  n = dim(M)[1]
  r <- round( (n/b) ,digits = 0) 
  col_length_M <-  dim(M)[2]
  i <- 0
  bands <- 0
  d <-0
  duplicates <- matrix(0,col_length_M,col_length_M)
  #op 2 moet b staan 
  for(bands in 0: b){
    band_index_1 <- (1 + bands*r) 
    band_index_2 <- (r + bands*r)
    band_matrix<- M[ c( band_index_1 : band_index_2 ) , c(1 : col_length_M )]
    #print(band_index_1)
    trans<- t(band_matrix)
    
    duplicate_index <- which(duplicated2(trans))
    number_of_dup <- length(which(duplicated2(trans) ))
    
    for(i in 1 : number_of_dup ){
      
      index_1 <- duplicate_index[i]
      
      for(d in i+1: number_of_dup ){
        
        index_2 <- duplicate_index[d]
        #print(duplicate_index)
        if(  identical(band_matrix[,index_1] ,band_matrix[,index_2] )){
          duplicates[index_1, index_2 ] <- 1
        }
      }
      
    }
  }
  return(duplicates)
}

lsh2 <- function(M , b ,r){
col_length_M <-  dim(M)[2]
i <- 0
bands <- 0
d <-0
duplicates <- matrix(0,col_length_M,col_length_M)
#op 2 moet b staan 
for(bands in 0 : (b) ){
  print(paste(bands , "of",b))
  
  band_index_1 <- (1 + bands*r) 
  band_index_2 <- (r + bands*r)
  if(band_index_2 > dim(M)[1]){
    return(duplicates)
  }
  else{
 # print(paste(band_index_1 , " indexen ",band_index_2))
  
  if(bands == (b -1)){
    band_matrix <- M[ c(( band_index_1 -1) : dim(M)[1] ) , c(1 : col_length_M )]
  }
  else{
    band_matrix <- M[ c( band_index_1 : band_index_2 ) , c(1 : col_length_M )]
  }
  #print(band_index_1)
  trans<- t(band_matrix)
  
  duplicate_index <- which(duplicated2(trans))
  number_of_dup <- length(duplicate_index)
  
  for(i in 1 : number_of_dup ){
    
    index_1 <- duplicate_index[i]
    
    for(d in (i+1) : number_of_dup ){
      
     # print(paste(i, " indexen ",d))
      index_2 <- duplicate_index[d]
    #  print(paste(index_1 , "         ",index_2))
      #print(duplicate_index)
      if(!is.na(index_1)  && !is.na(index_2) ){
      if(  identical(band_matrix[,index_1] ,band_matrix[,index_2] )){
        duplicates[index_1, index_2 ] <- 1
      }
      }
      }
  }
  }
}
return(duplicates)
}


lsh3 <- function(M_input , b_1 ,r_1){
  col_length_M_imput <-  1624
  i <- 0
  bands <- 0
  
  duplicates_1 <- matrix(0,col_length_M_imput,col_length_M_imput)
  
  #op 2 moet b staan 
  for(bands_1 in 0 : (b_1 - 1) ){
    print(paste(bands , "of", b_1 ))
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
      for(i in 1: ant_col){
        for(j in (i+1) : ant_col){
          print(j)
          if(identical( band_matrix_1[,i] , band_matrix_1[,j] ) ){
            duplicates_1[i, j ] <- 1
          }
        }
      }
  }
  return(duplicates_1)
}


combine_est <- function(duplicates, estimation_model_id){
  ben <- duplicates + estimation_model_id
  ben[ben < 2] <- 0
  ben[ben > 1] <- 1
  return(ben)
}

results = function(real,estimates){
  tp <- 0
  tn <- 0
  fn <- 0
  fp <- 0
  rows <- dim(real)[1]
  cols <- dim(real)[2]
  for(r in 1 : rows){
    for(c in r: cols){
      if(real[r,c] == 1 && estimates[r,c] ==1 ){
        tp = tp+1
      }
      if(real[r,c] == 0 && estimates[r,c] ==0 ){
        tn = tn+1
      }
      if(real[r,c] == 1 && estimates[r,c] ==0 ){
        fn = fn+1
      }
      if(real[r,c] == 0 && estimates[r,c] ==1){
        fp = fp+1
      }
    }
  }
  fuc1 = tp/(tp + fp)
  fuc2 = tp/(tp+ fn)
  
  pair_qualitie <- (tp / (sum(real)))
  
  pair_completnes <- tp / sum(estimates)
  
  F1_star= 2/((1/fuc1) + (1/fuc2))
  results <- c(F1_star,pair_qualitie,pair_completnes, tp, sum(estimates))
  return(results)
}

find_duplicates_in_estimates <- function(list){
  
  length <- length(list)
  matrix_with_dup <- matrix(0,length,length)
  for(r in 1 : length){
    for(c in r: length){
       if(identical(list[[r]], list[[c]])){
         matrix_with_dup[r,c] <- 1
         matrix_with_dup[r,r] <- 0
       }
    }
  }
    return(matrix_with_dup)
}

create_M = function(binary_matrix2, N, n_rows){
  
  number_colum <- matrix(0, N, n_rows)
  
  
  for (i in 1:N) {
    number_colum[i,] =  sample(c(1:n_rows))
    
  }
  
  M_in = matrix(0, N, 1624)
  
  # # calculate the signature matrix 
  for (i in 1:dim(M_in)[1]){
    
    
    for(j in 1:dim(M_in)[2]){
      
      M_in[i,j] = space_one(number_colum[i,], binary_matrix2[,j])
      
    }
  }
  return(M_in)
}

  
  