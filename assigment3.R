outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])
## This funtion first returns the best state according to the outcome 
##specified
best <- function(state, outcome) {
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
  filtered_data<-data[data$State==state,]
  if (outcome == "heart attack"){
    desired_col <- 11
  }else if (outcome == "heart failure"){
    desired_col <- 17
  }else{
    desired_col <- 23
  }
  final_data<-filtered_data[!is.na(filtered_data[,desired_col])]
  hosp_list<-final_data[which.min(data[,desired_col]),2]
  sorted_hosp_list<-sort(hosp_list)
  sorted_hosp_list[1]
}