#' Plot TURF analysis graph
#' 
#' Performs TURF analysis
#' 
#' @param x \code{\link{turf}} object
#' @param colour line colour
#' @param fontsize label size
#' @importFrom reshape2 melt
#' @return ggplot object
#' @export 
#' @examples
#' turfplot(vas)
#' turfplot(vas, colour="red")
#' turfplot(vas, colour="green", fontsize=2)

turfplot <- function(x, colour="blue", fontsize=3){
  
  .service <- y <- NULL # Trick to comply with R CMD check
  
  tab <- crossprod(as.matrix(x))
  tab <- tab - diag(diag(tab)) # drop the diagonal
  tab <- tab / nrow(x)         # convert to percentage
  tab <- tab * lower.tri(tab)
  tab <- as.data.frame(tab)
  tab$.service <- names(tab)
  
  # Melt data
  m <- melt(tab, id.vars=".service")
  #str(m)
  
  # Generate points on a circle
  m <- within(m, {
    service  <- as.factor(.service)
    variable <- as.factor(variable)
  })
  
  slabels <- levels(m$service)
  angles <- seq_along(slabels)
  angles <- angles / max(angles) * 2 * pi
  circle <- data.frame(
    x = cos(angles),
    y = sin(angles),
    names = slabels
  )
  
  m$x    <- circle$x[m$service]
  m$xend <- circle$x[m$variable]
  m$y    <- circle$y[m$service]
  m$yend <- circle$y[m$variable]
  
  # Discard all rows with no connections
  m <- m[m$value>0, ]
  
  ggplot(m, aes(x=x, y=y)) +
    geom_segment(aes_string(xend="xend", yend="yend", size="value"), colour=colour) +
    scale_size("Shared", range=c(0.5,2)) +
    coord_equal() +
    scale_x_continuous(limits=c(-1.3, 1.3)) +
    scale_y_continuous(limits=c(-1.3, 1.3)) +
    geom_text(data=circle, aes(x=1.2*x, y=1.2*y, label=gsub(" ","\n",names)), 
              size=fontsize, vjust=0.5) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none"
    )
}
