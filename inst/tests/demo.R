# TODO: Add comment
# 
# Author: Andrie
###############################################################################



# TURF analysis

# Attempt to conduct TURF analysis






###############################
# Main processing starts here #
###############################

#Loads data file
w2 <- readRDS(file=system.file("inst/data/TURFdata.rds", package="TURF"))
weight <- w2$weight
w2 <- w2[1:16,] #removes weight column from w2
nw2 <- names(w2)
ww2 <- nw2

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