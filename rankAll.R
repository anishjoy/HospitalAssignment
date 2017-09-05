
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

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  #read in the desired data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  #create a list of states and initialize a character array to hold the
  #required hospital names
  state <- unique(data[, 7])
  hospital <- vector(mode="character") 
  
  for (i in (seq(state)) {
    hospital[i] <- rankhospital(state[i], outcome, num)
  }
  data.frame(hospital, state)
}