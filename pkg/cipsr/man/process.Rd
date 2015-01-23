\name{process}

\alias{process}

\title{Processor algorithm for poles, saw-logs and chip-logs}

\description{
This function simulates processing of the harvested trees into different product classes.  Trees,
harvested during thinning or final felling, are bucked into poles, saw-logs, and chip-logs.  The bucking
algorithm prioritizes poles, then saw-logs, then chip-logs.  Conditions for harvesting can be specified
as a control argument.  Options for the control argument may be found in \code{processControl}. \cr

In most cases the process algorithm is efficient and produces results quickly.  It is suggested to use
the argument, ProgressBar=TRUE for exceptionally large treelists.  In these cases, the an estimate
of computing time is provided along with a progress bar. \cr
}

\usage{
process(treelist,control=list(),ProgressBar=FALSE)
}

\arguments{
\tabular{ll}{
\emph{treelist} \tab An output tree list from the \code{grow} function. \cr
\emph{control} \tab Arguments for processing each tree. \cr
\emph{ProgresBar} \tab Should a progress bar be made. By default FALSE. \cr
}
}

\author{Nathaniel Osborne \email{nathaniel.osborne@oregonstate.edu}}

\keyword{
tree harvesting algorithm
}