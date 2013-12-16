##AddTwo unit test

test_addtwo <- function(){
  n <- sample(x=((-10)^2):(10^2), size= 4)
  RANS <- n+2
  
  ANS <- AddTwo(n)
  
  stopifnot(is.numeric(ANS), length(ANS) == length(n), RANS==ANS)
}
