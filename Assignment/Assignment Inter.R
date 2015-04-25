Source(forecast)
Source(digest)
Source(foreign)
Source(TSA)

x<-read.table("data_scientist_assignment.tsv",TRUE)
#Extracting Table from the file


head(x)
str(x)
#Checking the information about the data set

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

z.inter<-c(1400,1459,2845,3003.3017,3128,3136)
#Vector representing the possible interventions


#Turning the non-stationary data into stationary data

z.log<-log(z)
#Box-Cox transformation with lambda=0

z.sdiff<-z.log[25:3264]
for(i in 25:3264){
  z.sdiff[i-24]<-z.log[i]-z.log[i-24]
}
#Seasonal differening



#Fitting a seasonal arima model SARIMA(5,0,0)(1,0,1)_12 without the intervention analysis
z.fitARIMA500101<-arima(z.sdiff, order=c(5L,0L,0L), seasonal=list(order=c(1L,0L,1L),period=24)) 

auto.arima(residuals(z.fitARIMA500101))
#Checking if the residuals are really white noise by checking if auto.arima to picks a 
#ARIMA(0,0,0) model for the residuals.

accuracy(z.fitARIMA500101)
#Checking different parameters of accuracy for the mdoel



predARIMA<-function(z,z.fitARIMA,id=200){
  #Function to produce the prediction of lth entry based on the data until (l-1)th entry  
  
  z.sdiffpred<-fitted.values(z.fitARIMA)[id-24]
  #To extract the fitted values from the models for z.sdiff
  
  output<-z[id-24]*exp(z.sdiffpred)
  #Calculating the prediction for the original time series
  
  output<-cbind("Original"=c(z[id]), "Predicted"=c(as.integer(output)), "Relative Error"=c((z[id]-output)/z[id]))
  
  output
  #Returning a vector giving orignial value and predicted value
  
}


predtableARIMA<-function(z,z.fitARIMA,id=23:437){
  #To create a table with orginal and predicted values with rows indexed by the entry number
  
  output<-cbind("Original"=c(),"Predicted"=c())
  #Creating an empty output for initialization
  
  
  for (i in id){
    
    output<-rbind(output,predARIMA(z,z.fitARIMA,i))
  }
  #Creating the output for the index given by l
  
  rownames(output)<-c(as.character(id))
  #Giving the rows names according to the index
  
  output<-as.data.frame(output)
  #converting output to data frame
  
  output<-cbind("Date"=as.matrix(as.factor(x$date[as.integer(id)])),"Hour"=as.matrix(as.integer(x$hr_of_day[as.integer(id)])),output)
  #adding columns for date and hour of the day
  
  output
  #returning the output as a row vector of length five with date, hour of the day, 
  #original value, predicted value, and relative error
}


predtableARIMA(z,z.fitARIMA,c(459L,623L,1223L,1827L,2122L,2530L,2984L,3100L))
#Predictions based on a test samples.

forecastARIMA<-function(z, z.fitARIMA,n=10){
  #To forecast 10 values using the model
  
  temp<-0
  output<-z
  #defining temporary elements to use the for forcasting using above functions 
  
  for (i in 1:n){
    temp<-predARIMA(output,z.fitARIMA,3264L+i)[,"Predicted"]
    output<-c(output,temp)
  }
  #Apending temp with the forecast one at a time
  
  output
  
}


z.FinalARIMA500101<-predtableARIMA(z,z.fitARIMA500101,70:3264)
#Making a final table for the predicted values compared to the original ones

write.table(z.FinalARIMA500101, file="Assignment/FinalARIMA500101.tsv", sep="\t")
#Storing the above table in the file "Assignment/FinalARIMA500101.tsv"

z.FinalARIMA500101$Error<-z.FinalARIMA500101$Original-z.FinalARIMA500101$Predicted
#Adding a column containing error in the predictions

head(z.FinalARIMA500101)
#Showing the head of the above table

z.FinalARIMA500101var<-mean((z.FinalARIMA500101$Original-z.FinalARIMA500101$Predicted)^2)
#Calculating the covariance


##########################Another Model##########################################

#Fitting a seasonal arima model SARIMA(5,1,1)(1,0,1)_12 without the intervention analysis
z.fitARIMA511101<-arima(z.sdiff, order=c(5L,1L,1L), seasonal=list(order=c(1L,0L,1L),period=24)) 

auto.arima(residuals(z.fitARIMA511101))
#Checking if the residuals are really white noise

accuracy(z.fitARIMA511101)
#Showing the accuracy of the fit for z.sdiff

predtableARIMA(z,z.fitARIMA511101,c(459L,623L,1223L,1827L,2122L,2530L,2984L,3100L))
#Showing a few of the predictions


z.FinalARIMA511101<-predtableARIMA(z,z.fitARIMA511101,70:3264)
#Making a final table for the predicted values compared to the original ones

write.table(z.FinalARIMA511101, file="Assignment/FinalARIMA511101.tsv", sep="\t")
#Storing the above table in the file "Assignment/FinalARIMA511101.tsv"

z.FinalARIMA511101$Error<-z.FinalARIMA511101$Original-z.FinalARIMA511101$Predicted
#Adding a column containing error in the predictions

head(z.FinalARIMA511101)
#Showing the head of the above table

z.FinalARIMA511101var<-mean((z.FinalARIMA511101$Original-z.FinalARIMA511101$Predicted)^2)
#Calculating the covariance


#############################Interventional Analysis############################

#Getting a seasonal arima model SARIMA(4,0,1)(1,0,1)_24 with interventions at 
#t=1400,1459,2845,3003.3017,3128,3136.
z.fitinterarima<-arimax(z.sdiff,order=c(4,0,1),seasonal=list(order=c(1,0,1),period=24),
                  xtransf=data.frame(I1400=1*(seq(z.sdiff)==1400),I1459=1*(seq(z.sdiff)==1459), 
                  I2845=1*(seq(z.sdiff)==2845),I3003=1*(seq(z.sdiff)==3003), I3017=1*(seq(z.sdiff)==3017),
                  I3128=1*(seq(z.sdiff)==3128),I3136=1*(seq(z.sdiff)==3136)),transfer=list(c(1,0),c(1,0),c(1,0),c(1,0),c(1,0),c(1,0),c(1,0)),
                  method='ML')

auto.arima(residuals(z.fitinterarima))
#Showing that the residuals follow ARIMA(0,0,0) and thus are white noise

accuracy(z.fitinterarima)
#Showing the accuracy of the fit for z.sdiff


predtableARIMA(z,z.fitinterarima,c(459L,623L,1223L,1827L,2122L,2530L,2984L,3100L))
#Showing a few of the predictions


z.Finalinterarima<-predtableARIMA(z,z.fitinterarima,70:3264)
#Making a final table for the predicted values compared to the original ones

write.table(z.Finalinterarima, file="Assignment/Finalinterarima.tsv", sep="\t")
#Storing the above table in the file "Assignment/Finalinterarima.tsv"

z.Finalinterarima$Error<-z.Finalinterarima$Original-z.Finalinterarima$Predicted
#Adding a column containing error in the predictions

head(z.Finalinterarima)
#Showing the head of the above table

z.Finalinterarimavar<-mean((z.Finalinterarima$Original-z.Finalinterarima$Predicted)^2)
#Calculating the covariance


##########################Another Model##########################################

#Fitting a seasonal arima model SARIMA(5,1,2)(1,0,1)_12 without the intervention analysis
z.fitARIMA512101<-arima(z.sdiff, order=c(5L,1L,2L), seasonal=list(order=c(1L,0L,1L),period=24)) 

auto.arima(residuals(z.fitARIMA512101))
#Checking if the residuals are really white noise

accuracy(z.fitARIMA512101)
#Showing the accuracy of the fit for z.sdiff

predtableARIMA(z,z.fitARIMA512101,c(459L,623L,1223L,1827L,2122L,2530L,2984L,3100L))
#Showing a few of the predictions


z.FinalARIMA512101<-predtableARIMA(z,z.fitARIMA512101,70:3264)
#Making a final table for the predicted values compared to the original ones

write.table(z.FinalARIMA512101, file="Assignment/FinalARIMA512101.tsv", sep="\t")
#Storing the above table in the file "Assignment/FinalARIMA512101.tsv"

z.FinalARIMA512101$Error<-z.FinalARIMA512101$Original-z.FinalARIMA512101$Predicted
#Adding a column containing error in the predictions

head(z.FinalARIMA512101)
#Showing the head of the above table

z.FinalARIMA512101var<-mean((z.FinalARIMA512101$Original-z.FinalARIMA512101$Predicted)^2)
#Calculating the covariance
