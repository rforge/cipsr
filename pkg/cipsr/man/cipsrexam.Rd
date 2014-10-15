\name{cipsrexam}
\alias{cipsrexam}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(cipsrexam)}
\format{
  The format is:
List of 3
 $ samples   :'data.frame':	63 obs. of  10 variables:
  ..$ unit   : chr [1:63] "A" "A" "A" "A" ...
  ..$ sample : num [1:63] 1 1 1 1 1 1 1 1 1 1 ...
  ..$ tree   : num [1:63] 0 0 0 0 0 0 0 0 0 0 ...
  ..$ expan  : num [1:63] 20 20 20 20 20 20 20 20 20 20 ...
  ..$ user   : num [1:63] 29 29 29 39 39 39 0 0 0 0 ...
  ..$ species: num [1:63] 202 202 202 202 202 202 202 202 202 202 ...
  ..$ dbh    : num [1:63] 12.2 10 10.3 13.6 9 16 12.1 8.5 12.7 13.3 ...
  ..$ tht    : num [1:63] 93 92 87 97 81 98 93 78 92 95 ...
  ..$ cr     : num [1:63] 0.387 0.379 0.381 0.333 0.387 0.395 0.55 0.394 0.415 0.486 ...
  ..$ radgro : num [1:63] 0 0 0 0 0 0 0 0 0 0 ...
 $ units     :'data.frame':	5 obs. of  40 variables:
  ..$ unit     : chr [1:5] "A" "B" "2" "12" ...
  ..$ latitude : num [1:5] 0 0 0 43 0
  ..$ longitude: num [1:5] 0 0 0 -123 0
  ..$ pptdd    : num [1:5] 0 0 0 0 0
  ..$ whc      : num [1:5] 0 0 0 0 0
  ..$ wantplot : num [1:5] 2 2 0 0 0
  ..$ wanttable: num [1:5] 1 1 0 0 0
  ..$ woodqual : num [1:5] 1 0 1 0 1
  ..$ variant  : num [1:5] 1 2 1 2 1
  ..$ version  : num [1:5] 3 2 1 1 1
  ..$ driver   : num [1:5] 0 0 0 1 0
  ..$ groyrs   : num [1:5] 50 50 50 50 50
  ..$ iseven   : num [1:5] 1 1 1 1 1
  ..$ partcut  : num [1:5] 0 0 0 0 0
  ..$ pastfert : num [1:5] 0 0 0 0 0
  ..$ stage    : num [1:5] 24 24 76 75 77
  ..$ bhage    : num [1:5] 20 20 71 70 72
  ..$ dfsi     : num [1:5] 125 125 78.8 0 97.5
  ..$ otsi     : num [1:5] 0 0 0 83.1 0
  ..$ dhcal    : num [1:5] 1 1 1 1 1
  ..$ ccal     : num [1:5] 1 1 1 1 1
  ..$ dgrocal  : num [1:5] 1 1 1 1 1
  ..$ triple   : num [1:5] 0 0 0 0 0
  ..$ maxsdi   : num [1:5] 1 1 1 1 1
  ..$ dfsdi    : num [1:5] 0 0 0 0 0
  ..$ wgsdi    : num [1:5] 0 0 0 0 0
  ..$ phsdi    : num [1:5] 0 0 0 0 0
  ..$ gdval    : num [1:5] 0 0 0 0 0
  ..$ ghval    : num [1:5] 0 0 0 0 0
  ..$ dfret    : num [1:5] 0 0 0 0 0
  ..$ genes    : num [1:5] 0 0 0 0 0
  ..$ snc      : num [1:5] 0 0 0 0 0
  ..$ core     : num [1:5] 0 0 0 0 0
  ..$ cftd     : num [1:5] 0 0 0 0 0
  ..$ cfsh     : num [1:5] 0 0 0 0 0
  ..$ logll    : num [1:5] 32 32 32 32 32
  ..$ logml    : num [1:5] 8 8 8 8 8
  ..$ logtd    : num [1:5] 6 6 6 6 6
  ..$ logsh    : num [1:5] 0.5 0.5 0.5 0.5 0.5
  ..$ logta    : num [1:5] 8 8 8 8 8
 $ activities:'data.frame':	8 obs. of  7 variables:
  ..$ unit   : chr [1:8] "A" "A" "A" "B" ...
  ..$ trigger: chr [1:8] "year" "year" "rel" "rel" ...
  ..$ when   : num [1:8] 24 24 70 40 81 300 300 120
  ..$ what   : chr [1:8] "thin" "fert" "thin" "thin" ...
  ..$ how    : chr [1:8] "below" "N" "uniform" "uniform" ...
  ..$ metric : chr [1:8] "rel" "pounds" "rel" "bap" ...
  ..$ target : num [1:8] 30 200 50 100 0.8 100 150 50
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(cipsrexam)
## maybe str(cipsrexam) ; plot(cipsrexam) ...
}
\keyword{datasets}
