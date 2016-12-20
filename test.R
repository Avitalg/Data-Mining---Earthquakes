#install.packages(c("ggplot2", "rworldmap"))
require(ggplot2)
require(rworldmap)
setwd("C:\\Users\\avital\\Documents\\R_Graph\\data mining course")

#get dataset fro csv
file<-"data3.csv"
Data<-read.csv(file, header=TRUE)
DF<-Data[,c("time","latitude","longitude", "depth","mag", "id", "type", "gap")]
mydata <- data.frame(DF)

#fix columns' type
mydata$time<- strptime(as.character(mydata$time), format = "%Y-%m-%dT%H:%M:%S")
mydata$id = as.character(mydata$id)

#cleaning
mydata<-mydata[mydata$type == 'earthquake',]        #only earthquake
mydata<-mydata[!duplicated(mydata$id),]             #unique id
rownames(mydata)<-mydata$id                         #change row name to it's iddi

#add timestamp column 
mydata$timestemp<-as.numeric(as.POSIXct(mydata$time, tz="GMT"))

#plot 
gapAmount<-mydata[mydata$gap>=180,]
gapPres<-nrow(gapAmount)/nrow(mydata)             #Calculates the presentage of gaps over 180
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-30, 59), ylim = c(35, 71), asp = 1)
points(mydata$longitude, mydata$latitude, col = "red", cex = .6)
