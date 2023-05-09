
#data1 <- data.frame(a=c(2L,4L,7L,9L),b=c(5,9,3,1),d=c(0,8,5,2),e=c(45L,89L,24L,13L),f=c("S","u","J","I"))

corFun<- function(a) {      
  data1 <- data.frame(a)
  type <- data.frame(col=sapply(data1,class))  #checking the classes of variables
  for(i in 1:nrow(type)){
    if(type$col[i]!="integer"){       #checking the classes of variables is integer
      if(type$col[i]!="numeric"){     #checking the classes of variables is numeric
        stop('Check the class of the dataFrame,It contains categorical/Factor variables')#excution stops
      }
    }
  } 
  data_names <- colnames(data1)       #taking column names of the data
  cmb <- combn(data_names,2)          #taking combinations of 2
  cmb_trans <- data.frame(t(cmb))       #transform the variabe names 
  names(cmb_trans) <- c("var1","var2")   #changing the names of the variable
  cmb_trans$correlation <- 0            #create new column with 0's
  for(j in 1:nrow(cmb_trans)){
  cor_out <- cor(data1[,cmb[1,j]],data1[,cmb[2,j]]) #finding the correlation of the combination
  cmb_trans$correlation[j] <- 0 + cor_out #for that combination the cor value is updating
  }
  cmb_trans1<- cmb_trans[cmb_trans$correlation >= 0.74 ,] #checking the condition which are greater than equal to 0.74
  return(cmb_trans1) #return the dataframe
  }


#corFun(vars_numeric)


