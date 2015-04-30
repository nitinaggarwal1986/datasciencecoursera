## Function to find the name of the hospital with rank 'num' in the 'state'
## mentioned, according to the factor 'outcome' mentioned


rankhospital<-function(state=character(), outcome=character(), num="best"){
  # Takes the state code and the outcome that will decide the ranking of the 
  # hospitals and which is one of "heart attack", "heart failure", or "pneumonia".
  
  a<-as.factor(read.csv("outcome-of-care-measures.csv", colClasses = "character")[,7])
  states<-levels(a)
  
  
  if (!sum(state==states)){
    stop("invalid state")
  }
  # Checking if the state argument is actually a state
  
  if (outcome=="heart attack"){
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")[,c(2,7, 11)]
  } else if (outcome=="heart failure"){
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")[,c(2,7, 17)]
  } else if (outcome=="pneumonia"){
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")[,c(2,7, 23 )]
  } else {
    stop("invalid outcome")
  }
  # Read the hospital.name, hospital.state, and 'outcome' columns of the outcome 
  # file.
  
  data<-data[data[,2]==state,]
  # Replacing whole data with data of the mentioned 'state'
  
  data<-data[order(data[,1]),]
  # Arranging the data in the alphabatical order with respect to the name of the
  # hospital
  
  suppressWarnings(data[,3]<-as.numeric(data[,3]))
  # coersing outcome value of the data from character to numberic
  
  data<-data[!is.na(data[,3]),]
  # Remove the hospital's data with the outcome not available.
  
  if(num!="best"&num!="worst"&num>nrow(data)){ return(NA)}
  # If the ranking for the input is more than the length of the data for the
  # particular state the return NA.
  
  if (num=="best"){
    num<-1
  }else if (num=="worst"){
    num<-nrow(data)
  }
  # Converting the "best" and "worst" cases to numeric entry.
  
  
  best.outcome<-NULL
  i<-0
  # initializing dummy variables 
  
  for(j in 1:as.integer(num)){
    
    data<-data[1:nrow(data)!=i,]
    
    best.outcome<-min(data[,3])
    
    i<-1
    while(data[i,3]!=best.outcome){i<-i+1}
    
  }
  # Calculating the index of the hospital with rank num.
  
  return(data[i,1])
  # returning the name of the hospital with the rank 'num' in the 'state' 
  # mentioned according to the 'outcome'
  
}