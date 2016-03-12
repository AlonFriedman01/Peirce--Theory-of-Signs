\name{Peirce-signN}
\alias{Peirce-signN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Runs the creation of N in layer 
}
\description{
Runs The Creation of Peirce object Value N.
}
\usage{
Peirce-signN(Sums, FullTable)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Sums}{
Sumed table from the Data being Tested
}
  \item{FullTable}{
 Full table is the full table of data that is being imported for testing.
}
}

\value{
The returned value is N in Peirce sign.
}

\author{
Alon Friedman
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
Peirce-Sign <- function(Sums,FullTable)
{
  N <- nrow(FullTable)
  lx <- Sums[3]
  ly <- Sums[4]
  xy <- Sums[5]
  x2 <- Sums[6]
  lx2 <- lx^2
  top <- (N*xy) - (lx*ly)
  bottom <- (N*x2) - (lx2)
  Nfinal <- top/bottom
  return(Nfinal)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
