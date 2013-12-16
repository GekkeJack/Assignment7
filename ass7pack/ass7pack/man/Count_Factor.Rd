\name{Count_Factor}
\alias{Count_Factor}
\alias{projectmanagers}
\title{
Count Factor function
}
\description{
The Count_Factor function counts unique character factors within a vector and return the counts in matrix form (in a data frame. Optionally you can sum of each unique factor.
}
\usage{
Count_Factor(vname,data,col.copy=NULL,cumulative = F)
}
\arguments{
  \tabular{ll}{
vname \tab is the column name of the vector within \emph{data} containing the character factors to be counted.\cr
data \tab is a data frame and the parent data set of the vector represented by \emph{vname}.\cr
col.copy \tab is numeric, but NULL by default. If a numeric value is supplied it should be <= the dimensions of \emph{data}; these columns are then copied to the data.frame that is returned by the function.\cr
cumulative \tab is logical and FALSE by default. If TRUE is supplied, the returned counts are summed per unique factor. \cr
}
}
\details{
\emph{vname} is the variable name of an array of factors within \emph{data} and should be supplied with parenthesis. 
Note that hereby it is assumed that \emph{data} is parent of \emph{vname}. Nevertheless, \emph{data = vname} is also allowed. \emph{vname} is then supplied without parenthesis. \cr\cr
\emph{col.copy} if not NULL, is numeric and within the range of column dimensions of data. It copies the specified column dimensions of data to the data frame that collects all the factor counts. For compatability it is best that the values are supplied using the concatenate function (c).\cr\cr
\emph{cumulative} is logical and gives the summed counts for each unique factor per project. This is especially usefull if the projects are chronologically listed. This is not the same as the colSums function from the base package because it gives the cumulative count over the factors(columns) per project (rows); i.e. from 1 to project i.  
}
\value{
The function returns a data frame matrix with [nrow(\emph{data}) x unique(\emph{vname})] dimensions and a count (1) for each time a unique factor of the \emph{vname} variable is found in each respective row of \emph{data}. When \emph{cumulative} is specified as TRUE, the counts are summed for each factor j over row 1 to i.
} 
\references{

}
\author{
T.H. van Balen (374663tb@eur.nl)
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

  Alpha <- c("A","B", "C", "D","E","F","G","H","A","B", "C", "D","E","F","G","H","A","B", "C", "D")
  Beta <- c("I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y", "Z", "AA","AB")
  testset <- matrix(c(Beta,Alpha),20,2)
  colnames(testset) <- c(paste("Projectnumber"), paste("Managers"))
  
  Count_Factor("Managers",testset,col.copy=1)
  Count_Factor(Alpha,Alpha)
  Count_Factor("Managers",testset,col.copy=1, cumulative = TRUE)
  
  data('projectmanagers')
  Count_Factor("PMWholeName", ProjectManagers,col.copy=1)
  Count_Factor("TechnologistWholeName", ProjectManagers,col.copy=1,cumulative=TRUE)
  
}


