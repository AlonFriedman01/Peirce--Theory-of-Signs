\name{Peirce-signXX}
\alias{Peirce-signXX}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multiplies Log base 10 of X by itself. (x^2)
}

\usage{
Peirce-signXX(Table)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Table}{
%%     ~~Describe \code{Table} here~~
}
}

\value{
Returned value is the result of X^2
}

\description{
This forumla is to create value of X^2
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
    value <- (Table[, 3:3] * Table[, 3:3])
    return(value)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
