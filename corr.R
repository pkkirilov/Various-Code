corr <- function(directory, threshold = 0) {
  files <- list.files(directory, full.names = TRUE)
  correlations <- numeric()
  for (i in 1:length(files)) {
    info <- read.csv(files[i])
    info <- info[complete.cases(info),]
    if (nrow(info) > threshold) {
      correlations[i] <- cor(info$sulfate, info$nitrate)
    }
  }
  bad <- is.na(correlations)
  correlations[!bad]
}
