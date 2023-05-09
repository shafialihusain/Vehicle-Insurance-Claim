
# test array:
# age <- c(5, 8, 10, 12, NA)
# a <- factor(c("aa", "bb", NA, "cc", "cc"))
# b <- c("banana", "apple", "pear", "grape", NA)
# df_test <- data.frame(age=age, a=a, b=b)
# df_test$b <- as.character(df_test$b)

#print(df_test)

#   age    a      b
# 1   5   aa banana
# 2   8   bb  apple
# 3  10 <NA>   pear
# 4  12   cc  grape
# 5  NA   cc   <NA>

Imputation <- function(a) {
  df_test <- data.frame(a)
  Mode <- function (x, na.rm) {
    xtab <- table(x)
    xmode <- names(which(xtab == max(xtab)))
    if (length(xmode) > 1) xmode <- ">1 mode"
    return(xmode)
  }
  for (var in 1:ncol(df_test)) {
    if (class(df_test[,var])=="numeric") {
      df_test[is.na(df_test[,var]),var] <- mean(df_test[,var], na.rm = TRUE)
    } else if (class(df_test[,var]) %in% c("character", "factor")) {
      df_test[is.na(df_test[,var]),var] <- Mode(df_test[,var], na.rm = TRUE)
    }
  }
  
  return(df_test)
  
}



# Imputation(age)
# Imputation(a)


