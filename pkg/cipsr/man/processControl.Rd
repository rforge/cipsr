\name{processControl}

\alias{processControl}

\title{Control values for the process function}

\description{
This list contains the default values used in the process function. These values can be modified and submitted
as an argument to the process function.  Note that all product lengths (e.x. sawll) must be given as whole numbers.
}

\usage{
processControl = list(pole,saw,chip,polell,poleml,poletd,polebd,
	sawll,sawml,sawtd,chipll,chipml,chiptd,sh,ta)
}

\arguments{
\tabular{ll}{
\emph{pole} \tab Attempt to cut poles from each tree. \cr
\emph{saw} \tab Attempt to cut saw logs from each tree. \cr
\emph{chip} \tab Attempt to cut chip logs from each tree. \cr
\emph{polell} \tab Maximum pole length to cut (ft). \cr
\emph{poleml} \tab Minimum pole length to cut (ft). \cr
\emph{poletd} \tab Minimum pole top diameter (in). \cr
\emph{polebd} \tab Minimum diameter 6-feet from the pole base (in). \cr
\emph{sawll} \tab Maximum saw log length (ft). \cr
\emph{sawml} \tab Minimum saw log length (ft). \cr
\emph{sawtd} \tab Minimum saw log top diameter (in). \cr
\emph{chipll} \tab Maximum chip log length (ft). \cr
\emph{sh} \tab Stump height (in). \cr
\emph{ta} \tab Trim allowance (in). \cr
}
}

\author{Nathaniel Osborne \email{nathaniel.osborne@oregonstate.edu}}

\keyword{
control, default values
}

\examples{
processControl()
}