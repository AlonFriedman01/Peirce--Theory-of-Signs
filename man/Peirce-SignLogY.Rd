\name{Peirce-signLogY}
\alias{Peirce-signLogY}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generates a new layer based on Table Log base 10 of Y
}

\usage{
Peirce-signLogY(Table)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Table}{
Table imported containing Papers and Authors.
}
}

\value{
Returned value is a Column containing Log base 10 of Y
}

\description{
This forumla is to create the Log base 10 of Y
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
    value <- log(Table[, 2:2], base = 10)
    return(value)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
