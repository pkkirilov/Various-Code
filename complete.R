complete <- function(directory, id = 1:332) {
  files <- list.files(directory, full.names = TRUE)
  dat <- data.frame()
  dat1 <- data.frame()
  for (i in id) {
    dat <- sum(complete.cases(read.csv(files[i])))
    sub <- data.frame (id = i, nobs = dat)
    dat1 <- rbind(dat1, sub)  
  }
  dat1
}