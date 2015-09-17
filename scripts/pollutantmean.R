pollutantmean <- function(directory, pollutant, id = 1:332) {
  dataset <- numeric()
  for(i in seq_len(length(id))) {
    append <- if(id[i]<10){
      "00"
    }else if (id[i]>9 && id[i]<100){
      "0"
    }else{
      ""
    }
    data <- read.csv(paste(directory, paste(append, id[i], ".csv", sep=""), sep="/"))
    dataset <- c(dataset, data[,pollutant])
  }
  mean(dataset, na.rm=T)
}