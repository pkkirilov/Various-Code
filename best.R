

best <- function(state, outcome) {
  out_data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)

  out_data <- out_data[,c(2, 7, 11, 17, 23)]
  names(out_data) <- c("hospital", "State", "heart.attack", "heart.failure", "pneumonia")

  check <- 0
  length1 <- nrow(out_data)
  for (i in 1:length1){
   if (out_data[i, 2] == state){
     check <- 1
   }
  }
  if (check < 1) {
    stop("invalid state")
  }
  z <- filter(out_data, State == state)

  z <- z[complete.cases(z),]
  if (outcome == "heart attack") { z <- arrange(z, heart.attack)}
  else if (outcome == "heart failure") { z <- arrange(z, heart.failure)}
  else if (outcome == "pneumonia") { z <- arrange(z, pneumonia)}
  else { stop("invalid outcome")}
  z[1, 1]
  #a <- sapply(out_data, "TX")
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}
