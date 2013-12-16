Haha <-
function(n){
  stopifnot(n > 0, is.numeric(n))
  Jokes <- list("Mathematics is made of 50 percent formulas, 50 percent proofs, and 50 percent imagination.","x squared asks x cubed: 'Do you believe in god?' To which x cubes replies: 'Well... I do believe in higher powers!'","Nono", "Two random variables were talking in a bar. They thought they were being discrete but I heard their chatter continuously.","Person 1: What's the integral of 1/cabin with respect to cabin? Person 2: A log cabin. Person 1: No, a houseboat; you forgot to add the C!", "There are only 10 types of people in the world: those who understand binary, and those who don't.","Nothing produces such odd results as trying to get even.","Question: Why did the mathematician refuse to eat the prime rib? Answer: Because it looked odd.")
  if(n %% 1 == 0){  
    if(n %% 2 == 0){
      if(n != 50 && n ==2){
        x <- paste(Jokes[2])
      }else{
        if(n == 50){
          x <- paste(Jokes[1])
        }else{
          if(n ==10){
            x <- paste(Jokes[6])
          }else{
            x <- paste("Question: Why is ", n, "+", n,"=",n+1, "like your left foot? Answer: Because it's not right.")
          }
        }
      }
    }else{
      if(n == 1){
        x <- paste(Jokes[5])
      }else{
        rand <- sample(c(7,8),1)
        x <- paste(Jokes[rand])
      }
    }
  }else{
    x <- paste(Jokes[4])
 }
  return(x)
}
