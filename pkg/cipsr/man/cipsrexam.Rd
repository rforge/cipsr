\name{cipsrexam}

\alias{cipsrexam}

\title{Example database to be used in the \code{grow} function}

\description{
In \code{cipsr}, forest data sets are stored as list objects, which are submitted as
an argument to the \code{grow} function. These lists are comprised of three elements: 
"samples", "units" and "activities".  The sample element contains measurements of 
individual trees.  The units element contains instructions for how a given unit in 
the sample list should be simulated by Organon or Cipsanon in the \code{grow} function.
The activities element contains instructions for silvicultural treatments to be imposed 
on a given unit. The object \code{cipsrexam} is an example dataset which could be 
supplied to \code{grow}.  Users should follow the format of \code{cipsrexam} when
preparing their databases. Alternatively, users may load an identically formatted 
Excel (.xls) file using \code{load.data}.   
}

\usage{
\code{grow(cipsrexam)} \cr
}

\format{
The example database consists of three list elements: "samples", "units" and "activities".
Each list element is data frame consisting of many columns.  The variables each column
represents are described below, but in much greater detail within the \code{cipsr} vignette. \cr
\cr
\bold{$samples} \cr
\describe{
	\item{unit}{identifier for the sampled stand (unit) of trees}
	\item{sample}{identifier for the sample nested within a unit}
	\item{tree}{identifier for tree in a sample and unit}
	\item{expan}{identifier for tree in a sample and unit}
	\item{user}{stand age to remove a given tree (year)}
	\item{species}{tree species identification code}
	\item{dbh}{diameter at breast height (inches)}
	\item{tht}{total tree height (feet)}
	\item{cr}{crown ratio}
	\item{radgro}{five-year radial growth (in)}
}
\cr
\cr
\bold{$units} \cr
\describe{
	\item{unit}{identifier for the sampled stand (unit) of trees}
	\item{latitude}{latitude (decimal degrees)}
	\item{longitude}{longitude (decimal degrees)}
	\item{pptdd}{precipitation for degree days greater than or equal to 41 degrees F; if left equal to zero, this value can be estimated by \code{cipsr}}
	\item{whc}{water holding capacity of the top 20 inches of soil; if left equal to zero, this value can be estimated by \code{cipsr}}
	\item{wantplot}{indicator to make a series of descriptive graphs}
	\item{wanttable}{indicator to produce a table of results outside of R}
	\item{woodqual}{indicator to estimate wood quality attributes of individual trees}
	\item{variant}{model variant to use in the simulation}
	\item{version}{version of a given model variant to use }
	\item{driver}{if Cipsanon is in use, define how to drive site productivity}
	\item{groyrs}{number of years to grow the unit}
	\item{iseven}{indicator for if the unit is even-aged}
	\item{partcut}{indicator that the unit has been partly harvested in the past}
	\item{pastfert}{indicator that the unit has been partly fertilized in the past}
	\item{stage}{total age of the unit (years); if uneven aged, this value must be zero}
	\item{bhage}{breast height age of the unit (years)}
	\item{dfsi}{Douglas-fir 50-year site index}
	\item{otsi}{other species site-index}
	\item{dhcal}{indicator for diameter and height calibration}
	\item{ccal}{indicator for crown ratio calibration}
	\item{dgrocal}{indicator for diameter growth calibration}
	\item{triple}{indicator to use tripling in the simulation}
	\item{maxsdi}{indicator for enforcing a maximum size-density limit}
	\item{dfsdi}{Douglas-fir maximum size-density index}
	\item{wgsdi}{White/Grand fir maximum size-density index}
	\item{phsdi}{Ponderosa pine/Western hemlock maximum size density index}
	\item{gdval}{Douglas-fir genetic worth value for diameter growth}
	\item{ghval}{Douglas-fir genetic worth value for height growth}
	\item{dfret}{needle retention of Douglas-fir infected by Swiss-needle cast}
	\item{genes}{use genetic worth values for diameter and height growth}
	\item{snc}{indicator for a unit inflected by Swiss-needle cast}
	\item{core}{definition of the juvenile wood code}
	\item{cftd}{top diameter inside for cubic foot volume estimation (inches)}
	\item{cfsh}{stump height for cubic foot volume estimation (feet)}
	\item{logll}{log length for Scribner volume estimation (feet)}
	\item{logml}{minimum log length for Scribner volume estimation (feet)}
	\item{logtd}{top diameter inside bark for Scribner volume estimation (inches)}
	\item{logsh}{stump height for Scribner volume estimation (feet)}	
	\item{logta}{trim allowance for Scribner volume estimation (inches)}	
}
\cr
\cr
\bold{$activities} \cr
\describe{
	\item{unit}{identifier for the sampled stand (unit) of trees}	
	\item{trigger}{type of unit condition used to trigger a silvicultural activity}	
	\item{when}{level of a unit condition to trigger a silvicultural activity}	
	\item{what}{silvicultural activity to be carried out if a trigger is met}	
	\item{how}{instructions for how a silvicultural activity should be carried out}	
	\item{metric}{type of unit condition used to define the residual target of a silvicultural activity}	
	\item{target}{level of residual condition desired from the silvicultural activity}	
}
}

\author{Nathaniel Osborne \email{nathaniel.osborne@oregonstate.edu}}

\seealso{
\code{\link{load.data}}
\code{\link{grow}}
}

\examples{
# Read more about formatting your data for use in cipsr
vignette("cipsr")

# Load the cipsr package 
library(cipsr)

# Explore the top few lines of the example database
lapply(cipsrexam,function(x){head(x)})

# Grow the example database; explore output 
grown = grow(cipsrexam)
lapply(grown,function(x){head(x)})
}

\keyword{
datasets
}