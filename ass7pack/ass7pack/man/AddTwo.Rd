\name{AddTwo}
\alias{AddTwo}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add Two, Add Four
}
\description{
The functions respectively add 2 and 4 to the supplied number (n).
}
\usage{
AddTwo(n)
AddFour(n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
  is a real number. 
}
}
\details{
n is numeric and can take any value on the real line between -inf and inf.

}
\value{
  \item{AddTwo}{adds 2 to the supplied number n and returns that value.
  }
  \item{AddFour}{adds 4 to the supplied number n and returns that value.
  }
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
T.H. van Balen (37463tb@eur.nl)
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x <- 2

AddTwo(x)
AddFour(x)


y <- 7.231

AddTwo(y)
AddFour(y)
}
