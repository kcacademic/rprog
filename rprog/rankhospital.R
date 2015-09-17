rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
  
  ## Check that state and outcome are valid
  validoutcomes <- c("heart attack", "heart failure", "pneumonia")
  validstates <- unique(as.vector(data$State))
  if (!is.element(state, validstates)) {
    stop("invalid state")
  }
  if (!is.element(outcome, validoutcomes)) {
    stop("invalid outcome")
  }
  if(num=="best"){
    num <- 1
  }
  
  ## Return hospital name in that state with the given rank 30-day death rate
  colnum <- if(outcome == "heart attack"){
    11
  } else if (outcome == "heart failure") {
    17
  } else if (outcome == "pneumonia"){
    23
  }
  subset <- subset(data[,c(2,7,colnum)], State==state)
  subset <- subset[complete.cases(subset),]
  subset <- subset[order(subset[,3],subset[,1]),]
  if(num=="worst"){
    num <- nrow(subset)
  }
  print(as.vector(subset[num,1]))
  
}