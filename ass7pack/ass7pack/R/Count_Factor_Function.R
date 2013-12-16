#Factor Count function

##PRECOND: vname is the variable name of an array of factors within data and should be supplied with parenthesis. 
#########  data is parent of vname. Data = vname is allowed, note that vname is then supplied without parenthesis. 
#########  col.copy if not NULL, is numeric and within the range of column dimensions of data. Copies the specified column dimensions of data to data.frame.
#########  cumulative is logical and gives the summed counts. 

##POSTCOND: matrix with [nrow(data) x unique(vname)] dimensions and a 1 for each time a unique value of vdata is found in data[j,]

Count_Factor <- function(vname,data,col.copy=NULL,cumulative = F){
  testname <- paste(vname)
  stopifnot(all(vname %in% colnames(data))||testname==data, is.logical(cumulative), is.character(testname))
    if(all(testname == data)){
      vdata <- as.factor(vname)
      dd <- length(data)  
      unique <- unique(vdata)
      dv <- length(unique)
      lcolc <- length(col.copy)
    }else{
      vdata <- as.factor(data[,vname])
      dd <- dim(data)  
      unique <- unique(vdata)
      dv <- length(unique)
      lcolc <- length(col.copy)
    }
  
  stopifnot(is.factor(vdata))
 
     Counts <- data.frame(mat.or.vec(dd[1],dv))

     for(i in 1:dv){
      Counts[,i] <- as.numeric(grepl(unique[i], vdata, fixed=TRUE, useBytes = FALSE))
     }

    if(cumulative==T){
      CumCounts <- mat.or.vec(dd[1],dv)
      
      for(i in 1:dd[1]){
        cl <- match(1, Counts[i,])
        CumCounts[i,cl] <- sum(Counts[1:i,cl])
      }
      Counts <- CumCounts
    } 
     if(lcolc != 0){
        stopifnot(is.numeric(col.copy))
        Counts <- data.frame(data[,col.copy],Counts)
        colnames(Counts)[1:lcolc] <- colnames(data)[col.copy]
        colnames(Counts)[(lcolc+1):(lcolc+dv)] <- paste(unique)
     }else{
       colnames(Counts)[1:dv] <- paste(unique)
     }
  
    
    return(Counts)
}