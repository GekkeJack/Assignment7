##AddFour unit test

test_addfour <- function(){
  n <- sample(x=((-10)^2):(10^2), size= 4)
  RANS <- n+4
  
  ANS <- AddFour(n)
  
  stopifnot(is.numeric(ANS), length(ANS) == length(n), RANS==ANS)
}
