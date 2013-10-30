\name{turfplot}
\alias{turfplot}
\title{Plot TURF analysis graph}
\usage{
  turfplot(x, colour = "blue", fontsize = 3)
}
\arguments{
  \item{x}{\code{\link{turf}} object}

  \item{colour}{line colour}

  \item{fontsize}{label size}
}
\value{
  ggplot object
}
\description{
  Performs TURF analysis
}
\examples{
turfplot(vas)
turfplot(vas, colour="red")
turfplot(vas, colour="green", fontsize=2)
}

