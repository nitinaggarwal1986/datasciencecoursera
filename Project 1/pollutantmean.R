pollutantmean<-function(directory,pollutant, id=1:332){
  
  
  if(is.character(directory)&length(directory)==1){  
    #Check if the 'directory' is character vector of length 1
    
    if(file.exists(directory)){    
      #check if the 'directory' is the location of a directory 
      
      if(is.character(pollutant)&length(pollutant)==1){
        #check if the 'pollutant is character vector of length 1
        
      
        if(pollutant=="sulfate"|pollutant=="nitrate"){ 
          #check if the pollutant is either sulfate or nitrate
        
          id<-as.integer(id)
          #convert id to integer array if it is not
        
          if(mean(id>=1&id<=332)==1){
            #check if id number is between 1 and 332
          
            file.names<-formatC(id,width=3, flag='0')
            file.names<-paste(directory,"/", file.names,".csv",sep="")
            #location of all tables with id numbers mentioned in 'id' 
          
           if(mean(file.exists(file.names))==1){
              #check if the files are actually at the correct location
            
             temp.table<-c()
             #initializing a temporary table
            
             reading<-c()
             
              for (i in 1:length(id)){
              
                temp.table<-read.csv(file.names[i],header=TRUE)
                reading<-c(reading,temp.table[[pollutant]])
                #apending the table temp.table with the table from file with id=i 
              }
              #making a single table for data from all monitors mentioned in id

              output<-mean(reading,na.rm=TRUE)
             

  
            
            }else stop("Cannot find all the files for mentioned id(s)")
          
          
          }else stop("Does not recognize id(s) other than those between 1 and 332")
        
        
        }else stop("Pollutant can either be sulfate or nitrate")
      
      }else stop("Pollutant is not a character vector of length 1")
      
    }else stop(paste(directory, "is not a directory"))
    
  } else stop(paste (directory, "is not a character vector of length one."))
  
  output
  #return mean
}
 