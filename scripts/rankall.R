rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
  
  ## Check that outcome are valid
  validoutcomes <- c("heart attack", "heart failure", "pneumonia")
  validstates <- unique(as.vector(data$State))
  if (!is.element(outcome, validoutcomes)) {
    stop("invalid outcome")
  }
  if(num=="best"){
    num <- 1
  }
  
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  colnum <- if(outcome == "heart attack"){
    11
  } else if (outcome == "heart failure") {
    17
  } else if (outcome == "pneumonia"){
    23
  }
  subset <- subset(data[,c(2,7,colnum)])
  subset <- subset[complete.cases(subset),]
  subset <- subset[order(subset[,2],subset[,3],subset[,1]),]
  
  dataframe <- setNames(as.data.frame(matrix(nrow = length(validstates), ncol = 2)), c('hospital', 'state'))
  i <- 1
  validstates <- sort(validstates)
  for(s in validstates){
    f <- subset(subset, State==s)
    print(nrow(f))
    if(num=="worst"){
      numc <- nrow(f)
    } else {
      numc <- num
    }
    dataframe$hospital[i]<-as.vector(f[numc,1])
    dataframe$state[i]<-s
    i <- i+1
  }
  dataframe
  
}