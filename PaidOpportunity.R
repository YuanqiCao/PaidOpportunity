library(scholar)
library(ggplot2)
library(tidyr)
library(plotly)
library(scholar)
library(tidyverse)
ids <- c("g_SNDWoAAAAJ&hl", "_kADnFMAAAAJ")

mr<-get_citation_history(ids[2])
mr[,2]<-(cumsum(mr[,2]))
plot(mr, type="l", 
     xlab="Year", ylab="Number of Citations",
     xlim=c(1995,2027), ylim=c(0,4000), xaxt='n')
axis(side=1, at=c(1995:2027), labels = c(1995:2027))

eoc<-get_citation_history(ids[1])
eoc[,2]<-(cumsum(eoc[,2]))
lines(eoc, col="red")

# # modeling
# modeoc<-glm(cites ~ year, data = eoc, family = "poisson")
# year<-2009:2025
# nd<-cbind(year,predict(object = modeoc,type = "response",newdata = data.frame(year=year)))
# lines(nd,  col="red", lty=2)
# 
# modemr<-glm(cites ~ year, data = mr, family = "poisson")
# year<-1996:2025
# nd<-cbind(year,predict(object = modemr,type = "response",newdata = data.frame(year=year)))
# 
# lines(nd, lty=2)
# abline(v=2019.35, col="blue")
# 

# modeling
modeoc<-loess(cites ~ year, data = eoc,
              control = loess.control(surface = "direct"))
year<-2009:2024
nd1<-cbind(year,predict(object = modeoc,type = "response",newdata = data.frame(year=year)))
lines(nd1,  col="salmon", lty=3)

modemr<-loess(cites ~ year, data = mr,
              control = loess.control(surface = "direct"))
year<-2007:2024
nd2<-cbind(year,predict(object = modemr,type = "response",newdata = data.frame(year=year)))

lines(nd2, col="gray",lty=3)


# legend("topleft", 
#        legend=c("EOC observed", "MR observed","EOC predicted GLM", 
#                 "MR predicted GLM","EOC predicted Loess", "MR predicted Loess",
#                 "Intersection GLM", "Intersection Loess"), 
#        col=c("red", "black", "red", "black", "salmon","gray", "blue","blue"), 
#        lty = c(1,1,2,2,3,3,1,2))

legend("topleft", 
       legend=c("EOC observed", "## observed","EOC predicted Loess", "## predicted Loess",
                "Intersection Loess"), 
       col=c("red", "black", "salmon","gray", "blue"), 
       lty = c(1,1,3,3,2))

year<-seq(2010, 2024, length.out = (2024-2010)*365)
nd1<-cbind(year,predict(object = modeoc,type = "response",newdata = data.frame(year=year)))
nd2<-cbind(year,predict(object = modemr,type = "response",newdata = data.frame(year=year)))
cross<-which.min(abs(nd1[,2]-nd2[,2]))
abline(v=year[cross[1]], col="blue", lty=2)
year[cross]
date<-as.Date(abs(floor(year[cross])-year[cross])*365, 
              origin = paste(floor(year[cross]),"-01-01", 
                             sep=""))
cat("catch up on", format(date, "%a %b %d %Y"),
    "at", nd1[cross[1],2],"citations","\n")

