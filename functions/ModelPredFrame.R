#### ModelPredFrame.R
#### Rudolf Cesaretti, 11/9/2022

#### "ModelPredFrame.R"
#### 
#### 
#### 
#### 

pak <- c("nls.multstart", "nlme", "nlstools")
# Install packages not yet installed
ip <- pak %in% rownames(installed.packages())
if (any(ip == FALSE)) {
  install.packages(pak[!ip])
}
# load packages
invisible(lapply(pak, library, character.only = TRUE))
rm(pak,ip)

###############################################################
######################  ModelPredFrame  #######################
###############################################################


ModelPredFrame <- function(nls_object, 
                           data, 
                           x, 
                           y, 
                           n=1024){
  q = as.data.frame(seq(from=min(data[,x]), to=max(data[,x]), length.out = n))
  names(q) = x
  z = predict(nls_object, newdata = q)
  q$z <- z
  names(q) = c(x,paste0(y,".p"))
  return(q)
}