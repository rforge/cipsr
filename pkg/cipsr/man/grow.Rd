\name{grow}

\alias{grow}

\title{Predict individual tree growth, yield and wood quality attributes}

\description{
The function \code{grow} predicts individual tree growth, yield and wood quality attributes. 
These predictions are based on instructions supplied in a list object, \emph{InputList}, 
which is formatted identically to \code{\link{cipsrexam}}.  To make predictions of future forest
conditions, the function \code{grow} calls a series of DLL files which are compiled from 
the Organon and Cipsanon Fortran source code written by David Hann, Doug Maguire and other members
of the Center for Intensive Planted-forest Silviculture at Oregon State University. \cr

The output from \code{grow} is a list object comprised of five data frames: \cr
\tabular{ll}{
\emph{treelist} \tab individual tree records referenced by stand age \cr
\emph{samplelist} \tab basic unit level statistics referenced by stand age \cr
\emph{woodquality} \tab individual tree attributes thought to be important to wood quality at the time of thinning and simulation conclusion \cr
\emph{standflags} \tab unit level model warnings and errors \cr
\emph{treeflags} \tab tree level model warnings and errors \cr
}
\cr
More detail about the \code{grow} function including input and output can be found in the cipsr vingette (i.e. \code{vingette("cipsr")}. 
}

\usage{
\code{grow(InputList)}
}

\arguments{
\tabular{ll}{
\emph{InputList} \tab A list of three dataframe objects labeled as $samples, $units and $activities.  See \code{\link{cipsrexam}} for an example. \cr
}
}
   
\author{Nathaniel Osborne \email{nathaniel.osborne@oregonstate.edu}}

\seealso{
\code{\link{cipsrexam}}
\code{\link{load.data}}
}

\examples{
# Load cipsr and make a prediction
library(cipsr)

# Grow the example dataset explore the output
grown = grow(cipsrexam)
lapply(grown,function(x){head(x)})
}

\keyword{
forest modeling, growth, yield, wood quality
}


