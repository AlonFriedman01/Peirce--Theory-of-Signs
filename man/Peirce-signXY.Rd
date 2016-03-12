\name{Peirce-signXY}
\alias{Peirce-signXY}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multiplies Log base 10 of X and Log base 10 of Y
}
\description{
Multiplies Log base 10 of X and Log base 10 of Y together and returns a table.
}
\usage{
Peirce-signXY(Table)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Table}{
%%     ~~Describe \code{Table} here~~
}
}

\value{
Returns a table of X*Y.
}

\author{
Alon Friedman
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (Table) 
{
    value <- (Table[, 3:3] * Table[, 4:4])
    return(value)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
