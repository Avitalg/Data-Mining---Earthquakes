#install.packages(c("ggplot2", "rworldmap"))
require(ggplot2)
require(rworldmap)
setwd("C:\\Users\\avital\\Documents\\R_Graph\\data mining course")

#get dataset fro csv
file<-"data.csv"
Data<-read.csv(file, header=TRUE)
DF<-Data[,c("time","latitude","longitude", "depth","mag", "id", "type", "gap")]
mydata <- data.frame(DF)
nrow(mydata)

#fix columns' type
mydata$time<- strptime(as.character(mydata$time), format = "%Y-%m-%dT%H:%M:%S")
mydata$id = as.character(mydata$id)

#cleaning
mydata<-mydata[mydata$type == 'earthquake',]        #only earthquake
mydata<-mydata[!duplicated(mydata$id),]             #unique id
rownames(mydata)<-mydata$id                         #change row name to it's id

nrow(mydata)

#add timestamp column 
mydata$timestemp<-as.numeric(as.POSIXct(mydata$time, tz="GMT"))
#longitude, latitude, depth, mag
#plot 
gapAmount<-mydata[mydata$gap>=180,]
gapPres<-nrow(gapAmount)/nrow(mydata)             #Calculates the presentage of gaps over 180
#map
newmap <- getMap(resolution = "high")
plot(newmap, xlim = c(-125, -100), ylim = c(25, 45), asp = 1, main="California Earthquakes")
points(mydata$longitude,mydata$latitude, col = "red", cex = .6)
#hist
old.par <- par(mfrow=c(3, 1), mar=c(4,0,0,0))
layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE), heights=c(1,3))

b<-boxplot(mydata$mag, horizontal=TRUE, axes=FALSE)
h<-hist(mydata$mag,  plot=F) 
plot(h, main="",  cex.lab = 1, cex.axis=0.8 , xlab="mag",
     labels=paste(round(100*h$counts/sum(h$counts),1),"%",sep=""), col="#4682B4") 
summary(mydata$mag)

dev.off()
