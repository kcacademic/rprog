complete <- function(directory, id = 1:332) {
  df<- data.frame(id=integer(length(id)), nobs=integer(length(id)))
  for(i in seq_len(length(id))) {
    append <- if(id[i]<10){
      "00"
    }else if (id[i]>9 && id[i]<100){
      "0"
    }else{
      ""
    }
    data <- read.csv(paste(directory, paste(append, id[i], ".csv", sep=""), sep="/"))
    dataset <- data[complete.cases(data),]
    df$id[i] <- id[i]
    df$nobs[i] <- nrow(dataset)
  }
  df
}