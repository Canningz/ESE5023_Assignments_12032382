Least_moves <- function(x){
  move=0
  while (x !=1) {
    if(x%%2 == 0){
      x <- x/2
    move <- move + 1
      
    }else{
      x <- x-1
      move <- move + 1
    }
  }
  print(move)
}

Least_moves(5)