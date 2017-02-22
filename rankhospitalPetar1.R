rankhospital <- function(state, outcome, num) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
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
    if (outcome == "heart attack") {
      z <- arrange(z, hospital)
      z <- arrange(z, desc(heart.attack))
    }
    else if (outcome == "heart failure") {
      z <- arrange(z, hospital)
      z <- arrange(z, desc(heart.failure))
      }
    else if (outcome == "pneumonia") {
      z <- arrange(z, hospital)
      z <- arrange(z, desc(pneumonia))
     }
    else { stop("invalid outcome")}
    data <- nrow(z)
    
    if (num == 'worst'){
      rank = data
    } else if (num == 'best'){
      rank = data[1]
    }
    else {
      rank <- as.numeric(as.character(rank))
      
    }
    
    rank <- 1:data
    rank <- as.data.frame(rank)
    z <- cbind(z, rank)
    z
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with lowest 30-day death
    ## rate
}
 