pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files

  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  sumv <- c()  
  for(f in id) {
    name <- paste(formatC(f, width=3, flag="0"),".csv",sep="")
    file_data <- read.csv(paste(directory,"/",name,sep=""))
    
    pollutant_data <- file_data[[pollutant]]
    pollutant_data <- pollutant_data[!is.na(pollutant_data)]
    sumv <- c(sumv,pollutant_data)
  }
  meanv <- mean(sumv,na.rm=TRUE)
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  meanv
}
