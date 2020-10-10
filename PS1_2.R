# Creating matrices 
a <- sample(0:50,50)
print(a)
M1 <- matrix(data=a, nrow = 5, ncol = 10)
print(M1)
M2 <- matrix(data=a, nrow = 10, ncol = 5)
print(M2)

# Multiplying matrices 
Matrix_multip <- function(M1,M2){
  M=matrix(0,nrow = 5 , ncol = 5)
  for(b in 1:5){
    for (c in 1:5) {
      for (d in 1:5) {
        M[b,c] <- M[b,c] + M1[b,d]*M2[d,c] 
      }
    }
  }
  print(M)
}

#ÑéÖ¤
Matrix_multip(M1,M2)
M1%*%M2

