##Count_Factor test

test_countfactor <- function(){
  Alpha <- c("A","B", "C", "D","E","F","G","H","A","B", "C", "D","E","F","G","H","A","B", "C", "D")
  Beta <- c("I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y", "Z", "AA","AB")
  testset <- matrix(c(Beta,Alpha),20,2)
  colnames(testset) <- c(paste("Projectnumber"), paste("Managers"))
  
  one <- diag(8)
  two <- diag(1,4,8)
  RANS <- cbind(Beta,rbind(one,one,two))
  
  ANS <- Count_Factor("Managers",testset,col.copy=1)
  stopifnot(all(ANS==RANS), colnames(ANS)==c(colnames(testset)[1],unique(Alpha)),is.data.frame(ANS))
  message("Test 1 success")
  
  one <- diag(8)
  two <- diag(2,8,8)
  three <- diag(3,4,8)
  RANS <- cbind(Beta,rbind(one,two,three))
  
  ANS <- Count_Factor("Managers",testset,col.copy=1, cumulative=T)
  stopifnot(all(ANS==RANS), colnames(ANS)==c(colnames(testset)[1],unique(Alpha)),is.data.frame(ANS))
  message("Test 2 success")

}
