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

Detail regarding the output from the process function may be found at in the cipsr vignette: help(package="cipsr").
}

\author{Nathaniel Osborne \email{nathaniel.osborne@oregonstate.edu}}

\keyword{
tree harvesting algorithm
}

\references{
Borders, B., & Shiver, B. (1995). Board Foot Volume Estimation for Southern Pines. 
	Southern Journal of Applied Forestry, 19(1).

Cascade, M. (2015). Standard Dimensions for Douglas Fir and Southern Yellow Pine Poles.

Hann, D. W. (2011). Revised Volume and Taper Equations for Six Major Conifer Species in 
	Southwest Oregon. Oregon State University, Department of Forest Engineering, Resources 
	and Management, Forest Biometrics Research Paper 2.

Maguire, D. (2014). Center for Intensive Planted-forest Silviculture: Annual Report 2013: 
	Models for the Height and Shape of the Heartwood Core in Douglas-fir. Oregon State 
	University, Department of Forest Engineering, Resources and Management.
	
Weiskittel, A. R., Maguire, D. a., & Monserud, R. a. (2007). Modeling crown structural responses 
	to competing vegetation control, thinning, fertilization, and Swiss needle cast in coastal 
	Douglas-fir of the Pacific Northwest, USA. Forest Ecology and Management, 245(1-3), 96–109. 
}

\examples{
grown = grow(cipsrexam)$treelist
harvest = process(grown,ProgressBar=TRUE) 
aggregate(cbind(cfv,bfv)~unit+type+class, data=harvest, FUN=sum)

}