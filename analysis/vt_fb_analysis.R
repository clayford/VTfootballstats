
# read in CSV data
setwd("~/GitHub/VTfootballstats/data")
vtds <- read.csv(dir()[1])
vtds[,1] <- as.factor(vtds[,1]) # season
vtds[,3] <- as.factor(vtds[,3]) # quarter

# check class of each variable
sapply(vtds[1,],class) 

# > names(vtds)
# [1] "season"  "opp"     "quarter" "result"  "top"     "plays"   "yards"   "outcome"


# some analysis
# mean yards per drive per quarter
ypq2012 <- aggregate(yards ~ quarter + season, data=vtds, mean, subset = season=="2012" & quarter!="5")

library(ggplot2)
ggplot(ypq2012, aes(x=quarter, y=yards)) + 
  geom_bar(stat="identity", fill="orange", color="black") +
  ggtitle("Mean yards per drive per quarter for 2012 season") +
  theme(plot.title=element_text(size=rel(1.5), face="bold"))


excl <- c("1994","1995","1996")

# TOP stats for wins versus losses (do not include 94 - 96 seasons)
topwl <- aggregate(top ~ outcome + season, data=vtds, mean, 
  subset = (season != "1994" & season != "1995" & season != "1996" & outcome != "tie"))
ggplot(topwl, aes(x=season,y=top, fill=outcome)) +
  geom_bar(stat="identity",position="dodge") +
  ggtitle("Mean time of possession per drive per season\nwins versus losses") +
  theme(plot.title=element_text(size=rel(1.5), face="bold"))

# TOP stats for wins versus losses (do not include 94 - 96 seasons) - total top per game per season
## PROBLEM WITH THIS; 2011 played Clemson twice, both losses, dataset does not distinguish between these two games!
topwl2 <- aggregate(top ~ season + opp + outcome, data=vtds, sum, 
   subset = (season != "1994" & season != "1995" & season != "1996" & outcome != "tie"))
topwl3 <- aggregate(top ~ outcome + season, data=topwl2, mean)
ggplot(topwl3, aes(x=season,y=top, fill=outcome)) +
  geom_bar(stat="identity",position="dodge") +
  ggtitle("Mean time of possession per game per season\nwins versus losses") +
  theme(plot.title=element_text(size=rel(1.5), face="bold"))


# Note: appears 1994 and 1995 do not have all games

table(vtds$outcome,vtds$season)

###################################################################
# OLD

# TOP
avgTOP <- round(mean(dsData2012$top[dsData2012$top > 0])) # drop TOP = 0, take mean, round
paste(avgTOP%/%60,":",avgTOP%%60,sep="") # convert to minutes and seconds (rough)
tapply(dsData2012$top,dsData2012$opp,mean) # avg TOP per game
tapply(dsData2012$top,dsData2012$opp,sum) # total TOP per game
tapply(dsData2012$top,dsData2012$opp,function(x){paste(sum(x)%/%60,":",sum(x)%%60,sep="")}) # total TOP per game, converted to m:s




# > sec <- time%%60
# > sec
# [1] 1
# > sprintf("%02d",sec)
# [1] "01"
# > time <- 146
# > sec <- time%%60
# > sec
# [1] 26
# > sprintf("%02d",sec)
# [1] "26"