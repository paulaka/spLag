\name{Lag.Matrix}
\alias{Lag.Matrix}
\title{
Generate spatio-temporal lags
}
\description{
This is a function to generate spatio-temporal lags for balanced samples. The function multiplies a predefined spatial weights matrix (e.g. use the Spatial.Mat function for this) with a longitudinal variable and lags the resulting vector by one time unit. The function also allows to generate seperate spatio-temporal lags for subgroups based on their size to model bi-directional spatial effects.  
}
\usage{
Lag.Matrix(input, W, time, obs, vartime, varid, asymmetriclow = FALSE, asymmetrichigh = FALSE, sizevar, size)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
longitudinal variable to be multiplied with the spatial weights matrix
}
  \item{W}{
spatial weights matrix 
}
  \item{time}{
number of repeated time points in longitudinal sample (must be balanced!)
}
  \item{obs}{
number of observations per time point (must be balanced!)
}
  \item{vartime}{
varible name containing time points, name must be put in quotation marks (e.g. variable name = week --> vartime ="week")
}
  \item{varid}{
varible name containing observation identifier, must be the same for each observation across all repeated time points. Name must be put in quotation marks (e.g. variable name = cityid --> vartime ="cityid")
}
  \item{asymmetriclow}{
TRUE = claculates only spatial-temporal lag for observations (e.g. communities) smaller than a certain size (e.g. population size), default = FALSE
}
  \item{asymmetrichigh}{
TRUE = calculates only spatial-temporal lag for observations larger than a certain size, default = FALSE
}
  \item{sizevar}{
variable in dataset containing the size of each observation (e.g. the population size of each community)
}
  \item{size}{
define size for cut-off 
}
}

\value{
lag = lagged spatia-temporal variable
}

\author{
Katrin Paula
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{

## The function is currently defined as
function (input, W, time, obs, vartime, varid, asymmetriclow = FALSE, 
    asymmetrichigh = FALSE, sizevar, size) 
{
    h <- input
    h <- reshape(h, idvar = varid, timevar = vartime, direction = "wide")
    for (i in 2:(time + 1)) {
        h[, i] <- W \%*\% h[, i]
    }
    h <- reshape(h, idvar = varid, direction = "long", timevar = vartime)
    h2 <- h[, 3]
    if (asymmetriclow == TRUE) {
        sizevar <- sizevar
        h2 <- ifelse(sizevar < size, h2, 0)
    }
    if (asymmetrichigh == TRUE) {
        sizevar <- sizevar
        h2 <- ifelse(sizevar > size, h2, 0)
    }
    start <- as.vector(rep(NA, times = obs))
    lag <- append(start, h2[1:(length(h2) - obs)])
    return(lag)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~manip }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~programming}% __ONLY ONE__ keyword per line
