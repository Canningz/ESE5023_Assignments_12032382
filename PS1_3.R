#loop for pascal_triangle
Pascal_triangle <- function(k) {
  Pas <- list()
  for (ij in 1:(k+1)) {
    Pas[[ij]] <- rep(1,ij)
  }
  if (k > 1) {
    for (i in 3:(k+1)){
      for (j in 2:(length(Pas[[i]])-1)){
        Pas[[i]][j] <- Pas[[i-1]][j-1] + Pas[[i-1]][j]
      }
    }
  }
  
  return(Pas)
}
#Report Pascal_triangle(100)
k <- 100
for (i in 1:(k+1)){
  for (j in 1:i){
    cat(format(Pascal(k)[[i]][j], width = 5))
  }
  cat("\n")
}
#Repor Pascal_triangle(200)
k <- 200
for (i in 1:(k+1)){
  for (j in 1:i){
    cat(format(Pascal(k)[[i]][j], width = 5))
  }
  cat("\n")
}