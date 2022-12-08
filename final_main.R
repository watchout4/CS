library(rjson)
library(purrr)
library(gdata)
library(tidyverse)
library(lsa)
library(bootstrap)
library(ggplot2)
library("rjson")
library(GLDEX)
## import data
path <- "~/Documents/uni/master/cs/TVs-all-merged.json"
#set to zero for all the data 
thinginvar <- 0
dummy_new   <-import_data_1(path)
model_id_new<-import_data_2(path)

#makes boot strp 
c<- c(1:1624)
length_round <- 1800
control <- c(1:1624)
boot_full <- sample(c , 1624, replace = T)

boot <- unique(boot_full)
#niet zeker

martijn_test <- c()

dummy_new_boot <- list()
out_of_sample_titel <- list()
out_of_sample_id <- list()
dummy_new_boot_model_id <-list()
#make the bootstrap sample in sample
for(i in 1: length(boot)){
  #the model id for the boot strap 
  index_5 <- boot[i]
  dummy_new_boot_model_id[i] <- model_id_new[index_5]
  #this gives the titel corresponing to the index
  index_3 <- boot[i]
  dummy_new_boot[i] <- dummy_new[index_3]
  #find the out of sample
  control[index_3] <- 0
}

boot <- unique(boot)
control <- fun.zero.omit(control)

# make the out of sample
i <- 0
for(i in 1: length(control)){
  index_4 <- control[i]
  out_of_sample_titel[i] <- dummy_new[index_4]
  out_of_sample_id[i] <- model_id_new[index_4]
}
This_one_id <- append(dummy_new_boot_model_id,out_of_sample_id)
This_one_title <- append(dummy_new_boot,out_of_sample_titel)

this_true_matrix <- true_nehbor_matrix(This_one_id)

this_mdl_ids_est <- extract_ids(This_one_title)

length(this_mdl_ids_est)

this_est_model_id <- estimation_model_id(this_mdl_ids_est)

dim(this_est_model_id)

This_dic <- dicksonary(This_one_title,0)

this_binary_matrix1 <- this_binary_matrix(This_one_title ,This_dic, 0)

this_M <- create_M( this_binary_matrix1, 1800 , 1624)

#M_all <- create_M(binary_matrix_all, length_round ,length(sample_in_and_out_titels))

M_all <-this_M
b_vector <- c(100,150,200,300,450,600)
f1_star_scores     <- matrix(0,length(b_vector) , 5)

f1_star_scores_dum <- matrix(0,length(b_vector) , 5)
lol3<-length(b_vector)

r_t_vector <- Calculate_r_t(b_vector , dim(M_all)[1])
r_vector <- r_t_vector[,1]
t_vector <- r_t_vector[,2]
lol3 <- length(b_vector)
#lol3 <- 1
for(i in 1: lol3 ){
  #duplicates_all <- lsh(M_all, b_vector[i])
  
  duplicates_all      <- lsh4(M_all, b_vector[i], r_vector[i])
  
  #deze code is er omdat ik een verkeerde index ergens had gepakt
  #duplicates_all <- duplicates_all[-c(2232:3776),-c(2232:3776)]
  combine_est_1_all     <- combine_est(duplicates_all, this_est_model_id)
  
  results_in_sample_all <- results( this_true_matrix, combine_est_1_all )
  results_dum           <- results( this_true_matrix, duplicates_all )
  
  f1_star_scores_dum[i,] <- t(results_dum)
  f1_star_scores[i,]     <- t(results_in_sample_all)
  
  print(results_dum)
  print(results_in_sample_all)
}

f1      <- c()
f1_goed <- c()
pl <- 100
for(i in 1:6){
  f1[i] <- (2 *f1_star_scores_dum[i,2]* (f1_star_scores_dum[i,3] *pl ) )/(f1_star_scores[i,2] + (f1_star_scores[i,3] ) )
}

for(i in 1:6){
  f1_goed[i] <- (2 *f1_star_scores[i,2]* ( f1_star_scores[i,3]  ) )/ (f1_star_scores[i,2] + (f1_star_scores_dum[i,3] ) )
}



colnames(f1_star_scores)     <- c("f1*", "PQ", "PC", "tp","Nc")
colnames(f1_star_scores_dum) <- c("f1*", "PQ", "PC", "tp", "nc")

fraction_of_compare <-c( )

plot_f1 <- c()

for(i in 1 : length(t_vector)){
  fraction_of_compare[i] <- 1 - t_vector[i]
}

tdf_results <tdf(f1_star_scores_dum , fraction_of_compare)

plot_f1 <- tdf_plot()

# with the extra combination step
qplot( fraction_of_compare, f1_star_scores[,2],xlab = "fraction of Comparisons", ylab = "PQ-measure", geom=c("point", "line"))
qplot( fraction_of_compare, f1_star_scores[,3], xlab = "fraction of Comparisons", ylab = "PC-measure", geom=c("point", "line"))
qplot( fraction_of_compare, f1_star_scores[,1],xlab = "fraction of Comparisons", ylab = "F1*-measure", geom=c("point", "line"))

#without the smart combination step 
qplot( fraction_of_compare, f1_star_scores_dum[,2],xlab = "fraction of Comparisons", ylab = "Pair quality",geom=c("point", "line"))
qplot( fraction_of_compare, (f1_star_scores_dum[,3] * 100),xlab = "fraction of Comparisons", ylab = "Pair completnes", geom=c("point", "line"))
qplot( fraction_of_compare, f1_star_scores_dum[,1],xlab = "fraction of Comparisons", ylab = "F1*-measure", geom=c("point", "line"))


qplot( fraction_of_compare, f1, xlab = "fraction of Comparisons", ylab = "F1*-measure", geom=c("point", "line"))
qplot( fraction_of_compare,  f1_star_scores_dum_lol[,3], xlab = "fraction of Comparisons", ylab = "PC-measure", geom=c("point", "line"))


qplot( fraction_of_compare,  f1_goed, xlab = "fraction of Comparisons", ylab = "F1", geom=c("point", "line"))
qplot( fraction_of_compare,  f1, xlab = "fraction of Comparisons", ylab = "F1", geom=c("point", "line"))

