
# the function keeps only the PCs that are necessary to explain at least 95% of the variability in the data,

#d=vars1_16

PCAfun <- function(d) {
  require(caret)
  data1 <- data.frame(d)
  type <- sapply(data1,is.numeric)  #checking the classes of variables
  data2 <- data1[,type]
  trans = preProcess(data2, 
                     method=c("BoxCox", "center", 
                              "scale", "pca"))
  PC = predict(trans, data2)
  return(PC)
}

#PCAfun(vars1_16)
