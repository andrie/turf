#' TURF analysis
#' 
#' Performs TURF analysis
#' 
#' @param x Named numeric vector
#' @param choices
#' @param weight
#' @return Vector
#' @export 
turf <- function(x, choices, weight=1)
{
	#x is a data frame consisting of numeric vectors [0;1]
	#choices is the size of combinations
	
	xnames <- names(x)
	# Generate list of combinations
	xcomb <- t(combn(xnames,choices))
	# Calculate number of combinations
	xcount <- choose(ncol(x),choices)
	
	y <- apply(as.array(1:xcount),1,function(i)
				(
							sum((as.numeric(
														apply(
																as.matrix(x[,xcomb[i,],]),1,
																sum)==choices)))
							)
	)
	
	y <- y / nrow(x)
	
#  xcomb <- t(xcomb)
#  y <- t(y)
	xcomb <- data.frame(xcomb,y)
	xcomb <- xcomb[order(xcomb[,choices+1],decreasing=TRUE),]
	rownames(xcomb) <- 1:nrow(xcomb)
	return(xcomb)
	
	
#  xcombinations<-rbind(xcombinations,y)
#  return(xcombinations[,order(xcombinations[choices+1,],decreasing=TRUE)])
}

#' Plot TURF analysis graph
#' 
#' Performs TURF analysis
#' 
#' @param w2 Results from TURF analysis
#' @return ggplot object
#' @export 
plot_turf <- function(w2){
	
	tab <- crossprod(as.matrix(w2))
	tab <- tab - diag(diag(tab)) # drop the diagonal
	tab <- tab / nrow(w2) # divide by the people to get percentages
	tab <- tab * lower.tri(tab)
	tab <- as.data.frame(tab)
	tab$service <- names(tab)
	
	# Melt data
	m <- melt(tab, id.vars="service")
	#str(m)
	
	
	# Generate points on a circle
	m$service <- as.factor(m$service)
	m$variable <- as.factor(m$variable)
	
	slabels <- levels(m$service)
	n <- length(slabels)
	angles <- 1:n
	angles <- angles / max(angles) * 2 * pi
#	circle <- dim(n)
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
	m <- m[m$value>0,]
	
	
	
	
	p <- ggplot(m,aes(x=x,y=y))
	p <- p + geom_segment(aes(xend=xend,yend=yend,size=value), colour="blue")
	p <- p + scale_size("Shared",to=c(0.5,2))
	p <- p + coord_equal()
	p <- p + scale_x_continuous(limits=c(-1.3,1.3))
	p <- p + scale_y_continuous(limits=c(-1.3,1.3))
	p <- p + geom_text(
			data=circle,
			aes(
					x=1.2*x,
					y=1.2*y,
					label=gsub(" ","\n",names)),size=3,
			vjust=0.5)
	theme_null <- theme_update(
			panel.grid.major = theme_blank(),
			panel.grid.minor = theme_blank(),
			axis.text.x = theme_blank(),
			axis.text.y = theme_blank(),
			axis.ticks = theme_blank(),
			axis.title.x = theme_blank(),
			axis.title.y = theme_blank(),
			legend.position = "none")
	
	p <- p + theme_set(theme_null)
	return(p)
}
