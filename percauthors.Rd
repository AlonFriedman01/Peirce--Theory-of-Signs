\name{percauthors}
\alias{percauthors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Runs a Percent of Authors Total from the table.
}
\description{
Creates a Percent Authors Table based on each column variable for authors against the total.
}
\usage{
percauthors(Table, Sums)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Table}{
The Table of data that is being tested,
}
  \item{Sums}{
The sum of data based off the Table,
}
}

\value{
The value returned is a table with percent Authors Table.
}

\author{
Kenneth Buker
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (Table, Sums) 
{
    justy <- Table[, 2:2]
    newcol <- justy/Sums[2]
    return(newcol)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
