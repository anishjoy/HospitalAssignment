
## This funtion first returns the best state according to the outcome 
##specified
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state is valid
  if (!state %in% unique(data$State[!is.na(data$State)])){
    stop("Invalid State")
  }
  ##Check outcome is valid
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop("Invalid Outcome")
  }
  ##Casting as Numeric
  data[,11]<-as.numeric(data[,11])
  data[,17]<-as.numeric(data[,17])
  data[,23]<-as.numeric(data[,23])
  ## Return hospital name in that state with lowest 30-day death
  ## rate

  if (outcome == "heart attack"){
    desired_col <- 11
  }else if (outcome == "heart failure"){
    desired_col <- 17
  }else{
    desired_col <- 23
  }
  filtered_data<-data[data$State==state & !is.na(data[,desired_col]),]
  if (is.numeric(num) == TRUE){
    if(length(unique(filtered_data[,2]))<num){
      return(NA)
    }
  }else{
    if (num == "best"){
      num <- 1
    }else if (num == "worst"){
      num <- length(unique(filtered_data[,2]))
    }else{
      return(NA)
    }
  }
  
  filtered_data<-filtered_data[order(filtered_data[,desired_col],filtered_data[,2]),]
  filtered_data[num,2]
}