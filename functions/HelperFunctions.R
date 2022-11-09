#### HelperFunctions.R 
#### Rudolf Cesaretti, 11/9/2022

#### "HelperFunctions.R"
#### 
#### 
#### 
#### 

###############################################################
######################  HelperFunctions  ######################
###############################################################

my_mean <- function(x) mean(x[x != 0],na.rm = TRUE)

my_median <- function(x) median(x[x != 0],na.rm = TRUE)
