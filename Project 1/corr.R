corr<-function(directory,threshold=0, id=1:332){
  
  
  if(is.character(directory)&length(directory)==1){  
    #Check if the 'directory' is character vector of length 1
    
    if(file.exists(directory)){    
      #check if the 'directory' is the location of a directory 
      
      if(is.numeric(threshold)&length(threshold)==1){
        #check if the 'threshold' is the numeric vector of length 1
      
        id<-as.integer(id)
        #convert id to integer array if it is not
      
        if(mean(id>=1&id<=332)==1){
          #check if id number is between 1 and 332
        
          file.names<-formatC(id,width=3, flag='0')
          file.names<-paste(directory,"/", file.names,".csv",sep="")
          #location of all tables with id numbers mentioned in 'id' 
        
          if(mean(file.exists(file.names))==1){
            #check if the files are actually at the correct location
          
            output<-as.numeric(c())
            #initializing a data frame for output with 0 columns and 0 rows
          
            temp.table<-1
            #initializing a temporary table
          
            for (i in 1:length(id)){
            
              temp.table<-read.csv(file.names[i],header=TRUE)
              #storing the table from file with id=i in temp.table for temporary use
              
              if(sum(complete.cases(temp.table))>threshold)
              {
                output<-c(output,cor(temp.table$sulfate,temp.table$nitrate,use="complete.obs"))
              }
            
            }
            #store the correlations between nitrate and sulfate of monitors passing the 
            #threshold requirements and store them as a numeric vector in output
            
          }else stop("Cannot find all the files for mentioned id(s)")
        
        }else stop("Does not recognize id(s) other than those between 1 and 332")
      
    }else stop("Threshold should be a numeric vector of length one")
    
    }else stop(paste(directory, "is not a directory"))
    
  }else stop(paste (directory, "is not a character vector of length one."))
  
  output

}