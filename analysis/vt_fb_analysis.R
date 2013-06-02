

##some stats##
sapply(ds2[1,],class) # check class of each variable

TOP
avgTOP <- round(mean(dsData2012$top[dsData2012$top > 0])) # drop TOP = 0, take mean, round
paste(avgTOP%/%60,":",avgTOP%%60,sep="") # convert to minutes and seconds (rough)
tapply(dsData2012$top,dsData2012$opp,mean) # avg TOP per game
tapply(dsData2012$top,dsData2012$opp,sum) # total TOP per game
tapply(dsData2012$top,dsData2012$opp,function(x){paste(sum(x)%/%60,":",sum(x)%%60,sep="")}) # total TOP per game, converted to m:s
