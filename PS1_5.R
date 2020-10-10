#5 Dynamic programming
#5.1
library(gtools)
input <- as.integer(runif(1,1,100))
Find_expression <- function(input){
  
  symbol <- c("+","-","")
  symbols <- permutations(3,8,symbol, repeats.allowed = TRUE)
  symbols <- cbind(symbols,"")
  num <- matrix(seq(1,9,by = 1),9,6561)
  num <- t(num)
  equa <- matrix("",6561,1)
  for( row in 1:6561 ){
    symbol1 <- c(symbols[row,])
    num1 <- c(num[row,])
    equa[row,] <- paste(num1,symbol1,sep = "",collapse = "")
  }
  count <- 0
  for( row in 1:6561 ){
    ans <- eval(parse(text = equa[row]))
    if (ans == input){
      print (equa[row])
      count <- count+1
    }
  }
  return(count)
}

#5.2
Total_solutions <- matrix(0,100,1)
for (input in 1:100){
  Total_solutions[input,]<- Find_expression(input)
}
plot(x = 1:100, y = Total_solutions,type = "l",)
maxnum = which(Total_solutions == max(Total_solutions))
minnum = which(Total_solutions == min(Total_solutions))
sprintf("%d yields the maximum of Total_solutions.", maxnum)
sprintf("%d yields the minimum of Total_solutions.", minnum)
#referto https://rosettacode.org/wiki/Sum_to_100
#referto ÇñÍúÍú