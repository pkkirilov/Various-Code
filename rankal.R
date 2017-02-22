rankall <- function(outcome, num = "best") {
  
  ## Read outcome data - and reduce table size
  fulltable <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available", stringsAsFactors=FALSE)
  
  ## Check that outcome are valid
  
  if (outcome %in% c("heart attack", "heart failure","pneumonia") == FALSE) {
    stop(print("invalid outcome"))
  }
  
  ## Return hospital name corresponding to the ranking for a given outcome
  
  ## 1. Subset data with state, rate and hospital name variables
  if (outcome == "heart attack") {col <- 11}
  else if (outcome == "heart failure") {col <- 17}
  else {col <- 23}
  smalldata <- fulltable[, c(2, 7, col)]
  colnames(smalldata) <- c("Hospital", "State", "Rate")
  smalldata[,3] <- suppressWarnings(as.numeric(smalldata[,3]))
  
  ## 2. Remove NAs
  clean <- complete.cases(smalldata)
  cleandata <- smalldata[clean, ]
  
  ## 3. Split and rank
  splitted <- split(cleandata, cleandata$State)
  
  output <- sapply(splitted, function(x) {   ##Define rank, and order lines
    if (num == "best") {rank <- 1}
    else if (num == "worst") {rank <- nrow(x)}
    else if (!(num < 1 || num > nrow(x))) {rank <- num}
    else if (num < 1 || num > nrow(x)){return(NA)}
    if (num < 1 || num > nrow(x)){print(NA)}
    
    subrank <- x[order(x[, 3], x[, 1]), ]
    linetoadd <-(subrank[rank, 1:2])
  }
  df <- data.frame(matrix(unlist(output), byrow=T), stringsAsFactors=FALSE)
}