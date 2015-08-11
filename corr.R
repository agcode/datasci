corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  rv <- numeric()
  for(f in 1:332) {
    comp <- complete("specdata",f)
    if(comp$nobs > threshold){
      name <- paste(formatC(f, width=3, flag="0"),".csv",sep="")
      file_data <- read.csv(paste(directory,"/",name,sep=""))
      s <- file_data$sulfate
      n <- file_data$nitrate
      cr <- cor(n,s,use="pairwise.complete.obs")
      rv<-c(rv,cr)
    }    
  }  
  rv
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
}
