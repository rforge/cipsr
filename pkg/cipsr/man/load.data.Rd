\name{load.data}

\alias{load.data}

\title{Tool to load input data}

\description{
Some users of \code{cipsr} will be more comfortable loading their data using a Microsoft Excel
template, rather than forming their input in R.  A tool, \code{load.data} has been made to 
load an Excel file, \emph{InputList}, into R.  The tool relies directly on the XLConnect package. \cr

An Excel file identical to \code{\link{cipsrexam}} called CIPSREXAM.xls has been provided as a template,
for \code{cipsr} users.  You may obtain this template using the function \code{\link{get.template}}. Once
you have the template located in your working directory, we suggest renaming this file to something more meaningful.
To use the template, clear and replace all information in the $sample, $units and $activities sheets of the 
Excel file. \cr

You can find more detail on loading your data into R using \code{load.data} in the \code{cipsr} vignette.  The 
best way to access the vignette is with the command: \code{help(package="cipsr")}. \cr
}

\usage{
load.data(...)
}
		   
\arguments{
\tabular{ll}{
\emph{InputFile} \tab The name of an Excel file to load.  Only .xls formats are accepted. \cr
}
}

\author{Nathaniel L. Osborne \email{nathaniel.osborne@oregonstate.edu}} 
	   
\seealso{
	\code{\link{cipsrexam}}
	\code{\link{grow}}
	\code{\link{get.template}}
}

\keyword{
data loader, XLConnect
}

\examples{
get.template()
dat = load.data("CIPSREXAM.xls")
sapply(dat,head)
}


