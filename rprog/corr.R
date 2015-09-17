corr <- function(directory, threshold = 0) {
  complete <- subset(complete(directory), nobs>threshold)
  y <- nrow(complete)
  cors <- numeric(y)
  for (i in seq_len(y)){
    x <- complete[i,1]
    append <- if(x<10){
      "00"
    }else if (x>9 && x<100){
      "0"
    }else{
      ""
    }
    data <- read.csv(paste(directory, paste(append, x, ".csv", sep=""), sep="/"))
    dataset <- data[complete.cases(data),]
    cors[i] <- cor(dataset[,"sulfate"],dataset[,"nitrate"])
  }
  cors
}