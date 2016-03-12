\name{Peirce-signLogX}
\alias{Peirce-signLogX}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate a New Layer (X from Table)
}
\description{
Generates a layer of Log base 10 of X
}
\usage{
Peirce-signLogX(Table)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Table}{
The table being imported containing classes and methods
}
}

\value{
Returns a Column with Log base 10 of X.
}

\author{
Friedman
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
add (Table) 
{
    value <- log(Table[, 1:1], base = 10)
    return(value)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
