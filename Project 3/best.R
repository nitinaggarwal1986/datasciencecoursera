## Function that finds the best hospital in the state according to the outcomes 
## which is one of "heart attack", "heart failure", or "pneumonia" mentioned. 

best<-function(state=character(), outcome=character()){
  # Takes the state code and the outcome that will decide the ranking of the 
  # hospitals and which is one of "heart attack", "heart failure", or "pneumonia".
  
  if (!sum(state==state.abb)){
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
  
  suppressWarnings(data[,3]<-as.numeric(data[,3]))
  # coersing outcome value of the data from character to numberic
  
  
  best.outcome<-min(data[,3], na.rm=TRUE)
  # Storing the best mortality rate because of the 'outcome' in the variable
  # best.outcome
  
  i<-1
  while(!is.na(data[i,3])&data[i,3]!=best.outcome|is.na(data[i,3])){i<-i+1}
  # Getting the index of the hospital with the best mortality rate.
  
  return(data[i,1])
  # Returning the name of the first hospital in the list with the best mortality 
  # rate for the specified outcome
}