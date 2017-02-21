#install.packages(c("ggplot2", "rworldmap", "caTools"))
require(ggplot2)
require(rworldmap)
require(caTools)

#importing dataset
setwd("C:\\Users\\avital\\Documents\\R_Graph\\Data-Mining-Earthquakes")
file<-"data.csv"
Data<-read.csv(file, header=TRUE)
DF<-Data[,c("time","latitude","longitude", "depth","mag", "id", "type", "gap")]
mydata <- data.frame(DF)

#fix columns' type and format
mydata$time<- strptime(as.character(mydata$time), format = "%Y-%m-%dT%H:%M:%S")
mydata$id = as.character(mydata$id)

#cleaning
mydata<-mydata[mydata$type == 'earthquake',]        #only earthquakes
mydata<-mydata[!duplicated(mydata$id),]             #unique id

#add timestamp column 
mydata$timestamp<-as.numeric(as.POSIXct(mydata$time, tz="GMT"))

#take only relevant variables
mydata<-mydata[,c("time","latitude","longitude", "depth","mag", "timestamp")]
n<-nrow(mydata)

#taking care of missing data
mydata$depth = ifelse(is.na(mydata$depth),
                      #Change empty data to avg
                      ave(mydata$depth, FUN = function(x)mean(x, na.rm = TRUE)),
                      mydata$depth)
mydata$mag = ifelse(is.na(mydata$mag),
                      #Change empty data to avg
                      ave(mydata$mag, FUN = function(x)mean(x, na.rm = TRUE)),
                    mydata$mag)

#shuffled dataset
shuffled<-mydata[sample(n),]

# Spliting the data into training set(70%) and test set(30%)
train<-shuffled[1:round(0.7*n),]
test<-shuffled[(round(0.7*n)+1):n,]

#feature scaling - normalization 
train[,2:6] = scale(train[,2:6])
test[,2:6] = scale(test[,2:6])

regressor = lm(formula = depth ~ mag, data = train)

#predict the test set results
y_pred = predict ( regressor, newdata = test)


#visualising the training set results
ggplot() +
  geom_point(aes(x = train$depth, y=train$mag), colour="red") +
  geom_line(aes(x=train$depth, y=predict(regressor, newdata = train)),colour="blue") +
  ggtitle('depth vs mag (training set)')

