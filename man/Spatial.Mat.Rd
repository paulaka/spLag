\name{Spatial.Mat}
\alias{Spatial.Mat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate different spatial weights matrices
}
\description{
This function allows to transform a simple point distance matrix to a variety of different spatial weights matrices.
}
\usage{
Spatial.Mat(DM, inverse = FALSE, decay = 1, rowstand = FALSE, bound = FALSE, bandlow = 0, bandupper, binary = FALSE, sqrt = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DM}{
input point distance matrix between observations
}
  \item{inverse}{
generates the inverse of each distance stored in the weights matrix, default = FLASE
}
  \item{decay}{
applies decay function to each distance stored in the weights matrix, default decay = 1
}
  \item{rowstand}{
rowstandardizes the weights matrix, default = FALSE
}
  \item{bound}{
creates bounded weights matrix, default = FALSE
}
  \item{bandlow}{
determines the lower band for the bounded weights matrix, default = 0
}
  \item{bandupper}{
determines the upper band for the bounded weights matrix
}
  \item{binary}{
transfers the bounded weights matrix into a binary bounded weights matrix, default = FALSE
}
  \item{sqrt}{
takes the square root of each distance stored in the weights matrix, default = FALSE
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Returns the W, which is the spatial weights matrix as defined above
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Katrin Paula
}
\note{
To use this function you need to create a simple point distance matrix between observations first. You can find an example how to calculate a distance point matrix using geographic points (longitude/latitude) in R here: \url{http://eurekastatistics.com/calculating-a-distance-matrix-for-geographic-points-using-r/}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%swerwe  o See Also as \code{\link{www.facebook.com}}, ~~~
%}
\examples{
## The function is currently defined as

function (DM, inverse = FALSE, decay = 1, rowstand = FALSE, bound = FALSE, 
    bandlow = 0, bandupper, binary = FALSE, sqrt = FALSE) 
{
    DM <- ifelse(DM == 0, NA, DM)
    if (bound == TRUE) {
        DM <- ifelse(DM != is.na(DM) & DM >= bandlow & DM < bandupper, 
            DM, NA)
        if (binary == TRUE) {
            DM <- ifelse(DM != is.na(DM), 1, DM)
        }
    }
    if (sqrt == TRUE) {
        DM <- ifelse(DM != is.na(DM), sqrt(DM), DM)
    }
    if (inverse == TRUE) {
        DM <- ifelse(DM != is.na(DM), DM^decay, DM)
        DM <- ifelse(DM != is.na(DM), 1/DM, DM)
    }
    if (rowstand == TRUE) {
        DM <- DM/apply(DM, 1, sum, na.rm = TRUE)
    }
    DM <- ifelse(is.na(DM), 0, DM)
    W <<- DM
    return(W)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~manip }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~programming }% __ONLY ONE__ keyword per line
