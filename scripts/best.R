best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
  
  ## Check that state and outcome are valid
  validoutcomes <- c("heart attack", "heart failure", "pneumonia")
  validstates <- unique(as.vector(data$State))
  if (!is.element(state, validstates)) {
    stop("invalid state")
  }
  if (!is.element(outcome, validoutcomes)) {
    stop("invalid state")
  }
  
  ## Return hospital name in that state with lowest 30-day death rate
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
  print(as.vector(subset[1,1]))
  
}