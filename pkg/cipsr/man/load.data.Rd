\name{load.data}

\alias{load.data}

\title{Tool to load input data}

\description{
Some users of \code{cipsr} will be more comfortable loading their data using a Microsoft Excel
template, rather than forming their input in R.  A tool, \code{load.data} has been made to 
load an Excel file, \emph{InputList} into R.  The tool relies directly on the XLConnect package. \cr

An Excel file identical to \code{\link{cipsrexam}} called CIPSREXAM.xls has been provided as a template,
for \code{cipsr} users.  To use the template, simply rename the Excel file to something meaningful and place
this file in your working directory.  Make sure the extension .xls of the Excel file is maintained.  Erase the 
example data contained in the Excel template and replace it with the information you wish to supply to the
\code{\link{grow}} function.  Definitions for each column in the $samples, $units and $activities sheets of 
CIPSREXAM.xls are described in detail in the \code{cipsr} vignette. \cr

Please ensure your computer has the latest version of Java installed.  Java is used to load the Excel file
by the XLConnect package.  Also, you may find the Excel template in the downloaded R package within the
folder named 'Example Data'. You can find the location of the \code{cipsr} package using the 
command, \code{path.package("cipsr")}.
}

\usage{
load.data(InputFile)
}
		   
\arguments{
\tabular{ll}{
\emph{InputFile} \tab The name of an Excel file to load \cr
}
}

\author{Nathaniel L. Osborne \email{nathaniel.osborne@oregonstate.edu}} 
	   
\seealso{
	\code{\link{cipsrexam}}
	\code{\link{grow}}
}

\examples{
# Ensure the Excel template is in the working directory
list.files(getwd()) 
}

\keyword{
data loader, XLConnect
}


