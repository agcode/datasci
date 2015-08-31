rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  if(!is.element(outcome,c("heart attack","heart failure","pneumonia"))){
    stop("invalid outcome")
  }
  ## For each state, find the hospital of the given rank
  states<-sort(unique(outcomedata[,"State"]))
  returnFrame<- data.frame(hospital= character(), state= character(),stringsAsFactors = FALSE)
  for(state in states){
    returnName <- ""
    if(outcome == "heart attack"){
      ##outcomedata<-outcomedata[order(outcomedata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,outcomedata$Hospital.Name, na.last = NA,decreasing = FALSE),]
      ##outcomedata<-outcomedata[order(as.numeric(outcomedata[, 11]),outcomedata[,2], na.last = TRUE,decreasing = FALSE),]
      ##row <- match(state,outcomedata$State)
      subdata<-outcomedata[outcomedata$State==state,]
      subdata<-subdata[order(as.numeric(subdata[,11]),subdata[,2], na.last = NA),]
      if(num=="best"){
        row<-1
        returnName<-subdata[row,"Hospital.Name"]
      }
      else if(num=="worst"){
        row<-nrow(subdata)
        ##returnName<-subdata
        returnName<-subdata[row,"Hospital.Name"]
      }
      else if(num<=nrow(subdata)){
        returnName<-subdata[num,"Hospital.Name"]
      }
      else if(num>nrow(subdata)){
        returnName<-NA
      }
      #returnName<-outcomedata[row,"Hospital.Name"]
    }
    if(outcome == "heart failure"){
      ##outcomedata<-outcomedata[order(outcomedata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,outcomedata$Hospital.Name, na.last = NA,decreasing = FALSE),]
      ##outcomedata<-outcomedata[order(as.numeric(outcomedata[, 17]),outcomedata[,2], na.last = TRUE,decreasing = FALSE),]
      ##row <- match(state,outcomedata$State)
      subdata<-outcomedata[outcomedata$State==state,]
      subdata<-subdata[order(as.numeric(subdata[,17]),subdata[,2], na.last = NA),]
      if(num=="best"){
        row<-1
        returnName<-subdata[row,"Hospital.Name"]
      }
      else if(num=="worst"){
        row<-nrow(subdata)
        returnName<-subdata[row,"Hospital.Name"]
      }
      else if(num<=nrow(subdata)){
        returnName<-subdata[num,"Hospital.Name"]
      }
      else if(num>nrow(subdata)){
        returnName<-NA
      }
      ##returnName<-outcomedata[row,"Hospital.Name"]
    }
    if(outcome == "pneumonia"){
      ##outcomedata<-outcomedata[order(outcomedata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,outcomedata$Hospital.Name, na.last = NA,decreasing = FALSE),]
      ##outcomedata<-outcomedata[order(as.numeric(outcomedata[, 23]),outcomedata[,2], na.last = TRUE,decreasing = FALSE),]
      ##row <- match(state,outcomedata$State)
      subdata<-outcomedata[outcomedata$State==state,]
      subdata<-subdata[order(as.numeric(subdata[,23]),subdata[,2], na.last = NA),]
      if(num=="best"){
        row<-1
        returnName<-subdata[row,"Hospital.Name"]
      }
      else if(num=="worst"){
        row<-nrow(subdata)
        returnName<-subdata[row,"Hospital.Name"]
      }
      else if(num<=nrow(subdata)){
        returnName<-subdata[num,"Hospital.Name"]
      }
      else if(num>nrow(subdata)){
        returnName<-NA
      }
      ##returnName<-outcomedata[row,"Hospital.Name"]
    }
    returnFrame<-rbind(returnFrame,data.frame(hospital = returnName, state = state))
    
  }
  ##data.frame(x = i, y = toString(i))
  ##returnFrame<-returnFrame[order(returnFrame$state, na.last = NA),]
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  returnFrame
}
