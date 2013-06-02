# drive summaries would be good to start with
# get last three columns: How Lost, Pl-Yds, TOP
library(XML) # needed to convert HTML table to list object

# I think it makes sense to get all IDs first then use those in the loop instead of looping through each season
allids <- data.frame(season=numeric(0),ID=character(0)) #will hold all box score ids; 322 games from 1987 - 2012
# new version using sub() function
for (i in 1987:2012){
  url <- paste("http://www.hokiesports.com/football/stats/",i,sep="")
  wp <- readLines(url)  
  box <- grep("showstats\\.html\\?[0-9]+",wp,value=TRUE) # split the element at "?"
  ids <- sub(".*?html\\?([0-9]{4,5}).*", "\\1", box)
  ids <- data.frame(season=i,ID=ids)
  allids <- rbind(allids,ids)
}
# allids

# this should work for the loop
# for (i in allids[,2])
# recall that allids has season  

# turns out there are no drive summaries for 1994 (all games?)
# 5881
# vtFballData <- function(start,stop,season){
dsf <- c()
# read the source code
j <- 1 # counter for season from allids
for (i in allids[,2]){
	url <- paste("http://www.hokiesports.com/football/stats/showstats.html?",i,sep="")
	web_page <- readLines(url)
  
  # see if drive summary is available, not always unfortunately
	if (length(grep("Virginia Tech Drive Summary", web_page)) == 0) {j <- j + 1; next} #increment counter to keep track of seasons
	
  # find where VT drive summary begins
	dsum <- web_page[(grep("Virginia Tech Drive Summary", web_page) - 2):(grep("Virginia Tech Drive Summary", web_page) + 18)]
	dsum2 <- readHTMLTable(dsum)
	rn <- dim(dsum2[[1]])[1]
	cn <- dim(dsum2[[1]])[2]
	ds <- dsum2[[1]][4:rn,c(1,(cn-2):cn)]
	ds[,3] <- as.character(ds[,3]) # convert Pl-Yds from factor to character
	# substitute space for first dash, split string at space (creates list object)
  # use do.call to assemble list elements into data frame
	py <- do.call(rbind,strsplit(sub("-"," ",ds[,3])," "))
	# add py dataframe (plays and yards) to ds
  ds2 <- cbind(ds,py)
	ds2[,5] <- as.character(ds2[,5]) # convert from factor to character
	ds2[,6] <- as.character(ds2[,6]) # convert from factor to character
	ds2[,5] <- as.numeric(ds2[,5]) # convert from character to numeric
	ds2[,6] <- as.numeric(ds2[,6]) # convert from character to numeric
	ds2[,3] <- NULL # drop original pl-yds column

	names(ds2) <-c("quarter","result","top","plays","yards")
	# drop unused factor levels carried over from readlines
	ds2$quarter <- ds2$quarter[, drop=TRUE] 
	ds2$result <- ds2$result[, drop=TRUE]

	# convert TOP from factor to character
	ds2[,3] <- as.character(ds2[,3]) 
	# convert TOP from M:S to just seconds
	ds2$top <- sapply(strsplit(ds2$top,":"),
  		function(x) {
    		x <- as.numeric(x)
    		x[1]*60 + x[2]})

	# need to add opponent
	opp <- web_page[grep("Drive Summary", web_page)]
	opp <- opp[grep("Virginia Tech", opp, invert=TRUE)] # get the one that is not VT
	opp <- strsplit(opp,">")[[1]][2]
	opp <- sub(" Drive Summary</td","",opp)
	ds2 <- cbind(allids[j,1],opp,ds2) # allids[j,1] gives the season
	dsf <- rbind(dsf,ds2)
  j <- j + 1 # counter to get season out of allids data frame
}

# change "allids[j, 1]" column header to "season"
names(dsf)[1] <- "season"

setwd("data")
write.csv(dsf,"vt_drive_summaries.csv", row.names = FALSE)



####################################################################################
# function to get the box score "showstats" ID numbers

# test <- readLines("http://www.hokiesports.com/football/stats/2011/")
# box <- grep("showstats\\.html\\?[0-9]+",test,value=TRUE)
# box1 <- unlist(strsplit(box,"\\?"))[c(FALSE,TRUE)] #get every second element
# ids <- substr(box1,1,5)

# seasons: 1987 - 2012 






# left over code for something else
####################################################################################
# find where play-by-play begins
grep("Play-By-Play", web_page)
[1] 1144

# get just the play-by-play section
# table code starts two lines above "Play-By-Play"
# don't need anything from Participation on down

test <- web_page[(grep("Play-By-Play", web_page) - 2):(grep("<h4>Participation</h4>", web_page)-1)]
test2 <- readHTMLTable(test)
