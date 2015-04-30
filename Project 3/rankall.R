## Function to return a data fram which has the ranking 'num' according to the
## different states measured with respect to the 'outcome' mentioned.

rankall<-function(outcome=character(), num="best"){
  # Takes the 'outcome' which decides the rank of the hospitals and the rank 
  # and return a dataframe with the Hospitals with statewise rank 'num'.
  
  if (outcome!="heart attack"& outcome!="heart failure" & outcome!="pneumonia"){
    stop ("invalid outcome")
  }
  # Check if outcome variable has a valid outcome and if not then returns an error
  
  output<-data.frame(hospital=c(),state=c())
  # initiallize the output
  
  a<-as.factor(read.csv("outcome-of-care-measures.csv", colClasses = "character")[,7])
  states<-levels(a)
  # Get a vector of all the states mentioned in the dataset
  
  for ( i in 1:length(states)){
    output<-rbind(output, data.frame(hospital=rankhospital(states[i],outcome, 
            num),state=states[i]))
  }
  # Create the output by adding rows with the name of the hospital with rank num
  # according to the states.
   
  rownames(output)<-as.character(states)
  # Naming the rows according to the state names.
  
  return(output)
  #returning the data frame with the hospital names with with statewise rank 'num'

  
}