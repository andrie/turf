# TODO: Add comment
# 
# Author: Andrie
###############################################################################



# TURF analysis

# Attempt to conduct TURF analysis



CreateFile <- function()
{
	library(foreign) # to import spss files
	filename<-"F:/my dropbox/pentalibra/clients/ofcom/r/wholesale.sav"
	w <- read.spss(filename)
	w<-as.data.frame(w)
	winc <- as.data.frame(read.csv("Phase 2 include list.csv"))
	
	#####################################################
	# Include only observations that are listed in winc #
	#####################################################
	wmatch <- apply(as.array(w$iobs),1,
			function(x) match(x,winc$Prodata.ID))
	
	w <- w[which(!is.na(wmatch)),]
	
#  wmatch <- apply(as.array(w$iobs),1,
#    function(x) match(x,winc$Prodata.ID))
#
	#########
	
	q <- paste("q1.5_",1:99,sep="")
	
	w1 <- w[q]
	w2<-NULL
	w2$q1.5_1 <- w$q1.5_1
	q<-1:99
	for (i in 2:99)
	{
		if (class(w1[q[i]][[1]]) == "factor")
		{
			w2<-cbind(w2,w1[q[i]])
		}
	}
	#Replaces factor with numeric values of 0 and 1
	
	for (i in 1:length(names(w2)))
	{
		names(w2)[i] <- levels(w2[[i]])[2]
		w2[i]<-as.numeric(w2[[i]])-1
	}
	
	w2$weight<-w$weight
	
	save(w2,file="D:/adevries/r/TURF/TURFdata.rda")
	rm(w1)
	rm(w2)
	rm(q)
	rm(i)
}



###############################
# Main processing starts here #
###############################

#Creates data file
#CreateFile()
#Loads data file
load(file="F:/my dropbox/pentalibra/r/TURF/TURFdata.rda")
weight<-w2$weight
w2<-w2[1:16,] #removes weight column from w2
nw2<-names(w2)
ww2<-nw2

# Calculate the means, sort descending and display on a barplot
#x<-sort(mean(w2),decreasing=TRUE)
#bp<-barplot(x[1:10],horiz=TRUE,beside=TRUE,las=1)
#text(0,bp,names(x),pos=4)

# Calculate combinations
TURFa <- turf(w2,2)
TURFb <- turf(w2,3)
TURFc <- turf(w2,4)

# Print results
print(TURFa[1:6,])
print(TURFb[1:6,])
print(TURFc[1:6,])


################################################################################

#library(reshape)
#library(ggplot2)

# Loads pre-saved data
# data file consists of a single data frame, called w2

load(file="F:/my dropbox/pentalibra/r/TURF/TURFdata.rda")
plot_turf(w2)