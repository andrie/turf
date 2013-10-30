\name{turf}
\alias{turf}
\title{TURF analysis}
\usage{
  turf(x, choices)
}
\arguments{
  \item{x}{numeric array with values 0 or 1}

  \item{choices}{Number of combinations}
}
\value{
  Data frame
}
\description{
  Performs TURF analysis
}
\examples{
turf(vas, choices=2)
turfplot(vas)
}
\seealso{
  turfplot
}

