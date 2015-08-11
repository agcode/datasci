complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  c_data <- data.frame(id = numeric(), nobs = numeric())
  for(f in id) {
    name <- paste(formatC(f, width=3, flag="0"),".csv",sep="")
    file_data <- read.csv(paste(directory,"/",name,sep=""))
    ctr <- 0
    for(x in 1:nrow(file_data)){
      if(anyNA(file_data[x,]) == FALSE){
        ctr <- ctr+1
      }
    }
    c_data <- rbind(c_data,data.frame(id=f,nobs=ctr))
  }
  c_data
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
}
