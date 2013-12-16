##Test Haha function

test_haha <- function(){
  n <- sample(1:(10^5), 1)
  
  Smile <- Haha(n)
  stopifnot(is.funny(Smile), is.character(Smile), grepl(n,Smile))
}