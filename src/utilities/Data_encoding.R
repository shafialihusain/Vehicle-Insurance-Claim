Data_encoding<- function(sampledata_df)
{
  input_df <- data.frame(sampledata_df)
  factors_input <- sapply(input_df, is.factor)
  vars_categ_input <- input_df[, factors_input]
  vars_numeric_input <- input_df[,!factors_input]
  
  new_col_encoding <- data.frame()
  new_col_encoding <- NA
  #i=2
  for(i in 1:ncol(vars_categ_input)){
    print(i)
    tr <- c(as.character(vars_categ_input[,i]))
    tr_uniq <- data.frame(unique=unique(tr))
    tr_uniq$unique <- as.character(tr_uniq$unique)
    tr_uniq_order <- transform(tr_uniq, n=nchar(as.character(tr_uniq)))
    lasy <- tr_uniq_order[with(tr_uniq_order, order(n, tr_uniq)), ]
    encoding <- data.frame(element=lasy[,1],number=1:nrow(lasy))
    new_col <- data.frame(element=as.character(vars_categ_input[,i]))
    library(plyr)
    new_col1 <- data.frame(new_col=mapvalues(new_col$element, from=encoding$element, to=encoding$number))
    new_col_encoding <- cbind(new_col_encoding,new_col1)
  }
  
  new_col_encoding<-new_col_encoding[,-1]
  for(i in 1:length(new_col_encoding)){
   # rename(new_col_encoding,   colnames(vars_categ_input[i]) = colnames(new_col_encoding[i]))
    names(new_col_encoding)[i]<-colnames(vars_categ_input[i])
  }
  new_col_encoding<-new_col_encoding
  output_df <- cbind(vars_numeric_input, new_col_encoding)
  return(output_df)
}