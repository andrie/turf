#' TURF analysis
#' 
#' Performs TURF analysis
#' 
#' @param x numeric array with values 0 or 1
#' @param choices Number of combinations
#' @return Data frame
#' @seealso turfplot
#' @export 
#' @examples
#' turf(vas, choices=2)
#' turfplot(vas)
turf <- function(x, choices){	
	xnames <- colnames(x)
	# Generate list of combinations
	xcomb <- t(combn(xnames, choices))
	# Calculate number of combinations
	xcount <- choose(ncol(x), choices)
	
	y <- apply(as.array(1:xcount), 1 ,function(i){
	  sum((as.numeric(
	    apply(x[, xcomb[i, ]], 1, sum)==choices)
	  ))
	})
	
	y <- y / nrow(x)
	
	xcomb <- data.frame(xcomb, y)
	xcomb <- xcomb[order(xcomb[, choices + 1], decreasing=TRUE), ]
	rownames(xcomb) <- 1:nrow(xcomb)
  class(xcomb) <- c("turf", "data.frame")
	return(xcomb)
}

