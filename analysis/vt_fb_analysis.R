
# read in CSV data
setwd("~/GitHub/VTfootballstats/data")
vtds <- read.csv(dir()[1])
vtds[,1] <- as.factor(vtds[,1]) # season
vtds[,3] <- as.factor(vtds[,3]) # quarter

# check class of each variable
sapply(vtds[1,],class) 

# some analysis
# mean yards per drive per quarter
ypq2012 <- aggregate(yards ~ quarter + season, data=vtds, mean, subset = season=="2012" & quarter!="5")

library(ggplot2)
ggplot(ypq2012, aes(x=quarter, y=yards)) + 
  geom_bar(stat="identity", fill="orange", color="black") +
  ggtitle("Mean yards per drive per quarter for 2012 season") +
  theme(plot.title=element_text(size=rel(1.5), face-"bold.italic"))



###################################################################
# OLD

# TOP
avgTOP <- round(mean(dsData2012$top[dsData2012$top > 0])) # drop TOP = 0, take mean, round
paste(avgTOP%/%60,":",avgTOP%%60,sep="") # convert to minutes and seconds (rough)
tapply(dsData2012$top,dsData2012$opp,mean) # avg TOP per game
tapply(dsData2012$top,dsData2012$opp,sum) # total TOP per game
tapply(dsData2012$top,dsData2012$opp,function(x){paste(sum(x)%/%60,":",sum(x)%%60,sep="")}) # total TOP per game, converted to m:s
