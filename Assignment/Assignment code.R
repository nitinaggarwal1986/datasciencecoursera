Source(forecast)
Source(digest)

x<-read.table("data_scientist_assignment.tsv",TRUE)
#Extracting Table from the file


head(x)
str(x)
Time <-paste(paste(x$date,x$hr_of_day), ":00:00",sep="")
Time2 <-as.POSIXlt(Time, format="%Y-%m-%d %H:%M:%S")
x$Time<-Time2
rm(Time)
rm(Time2)
x$date<-as.Date(x$Time)
#Optional code for adding a time element to the data

z<-ts(x$vals,frequency=24)
plot(z)
#Saving the time series in the ts format recognized by R

z.log<-log(z)
#Box-Cox transformation with lambda=0

z.sdiff<-z.log(25:3264)
for(i in 25:3264){
  z.sdiff[i-24]<-log(z[i])-log(z[i-24])
}
#Seasonal differening

z.fitarima29<-Arima(z.sdiff, order=c(29L,0L,0L)) 
#ARIMA(29,0,0) or AR(29) model fitting

z.fit<-z.fitarima29$coef
#Extraction of coefficient from the model

pred<-function(z,z.fit,l=200){
  #Function to produce the prediction of lth entry based on the data until (l-1)th entry  
  s<-0
  #initializing the sum in the model
  
  for (i in 1:29)
  {
    s<-s+(log(z[l-i])-log(z[l-i-24]))*z.fit[i,]
  }
  #calculating that sum
  
  op<-exp(log(z[l-24])+s+z.fit[30,])
  #Calculating the prediction
  
  op<-cbind("Original"=c(z[l]), "Predicted"=c(as.integer(op)), "Relative Error"=c((z[l]-op)/z[l]))
  op
  #Returning a vector giving orignial value and predicted value
  
}

predtable<-function(z,z.fit,l=23:437){
  #To create a table with orginal and predicted values with rows indexed by the entry number
  
  output<-cbind("Original"=c(),"Predicted"=c())
  #Creating an empty output for initialization
  
  
  for (i in l){
    
    output<-rbind(output,pred(z,z.fit,i))
  }
  #Creating the output for the index given by l
  
  rownames(output)<-c(as.character(l))
  #Giving the rows names according to the index
  
  output<-as.data.frame(output)
  #converting output to data frame
  
  output<-cbind("Date"=as.matrix(as.factor(x$date[as.integer(l)])),"Hour"=as.matrix(as.integer(x$hr_of_day[as.integer(l)])),output)
  #adding columns for date and hour of the day
  
  output
}

predtable(z,z.fit,c(234L,459L,623L,1223L,1827L,2122L,2530L,2984L,3100L))
#Predictions based on a test samples.

