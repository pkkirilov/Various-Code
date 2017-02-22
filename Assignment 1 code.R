pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
}

##source("pollutantmean.R")
##pollutantmean("specdata", "sulfate", 1:10)
## [1] 4.064
##pollutantmean("specdata", "nitrate", 70:72)
## [1] 1.706
##pollutantmean("specdata", "nitrate", 23)
## [1] 1.281

##part1
pollutantmean <- function(directory, pollutant, id = 1:332) {
  files <- list.files(directory, full.names = TRUE)
  dat <- data.frame()
    for (i in id) {
      dat <- rbind(dat, read.csv(files[i]))  
  }
  mean (dat[, pollutant], na.rm = TRUE)
}

##part 2 - check how to insert column and row headers; 
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

  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
}
#part 3
corr <- function(directory, threshold = 0) {
  files <- list.files(directory, full.names = TRUE)
  correlations <- numeric()
  for (i in 1:length(files)) {
    info <- read.csv(files[i])
    info <- info[complete.cases(info),]
    #info1 <- complete.cases(info)
    if (nrow(info) > threshold) {
      correlations[i] <- cor(info$sulfate, info$nitrate)
    }
  #else {
  #    return(numeric())
   # }
    
  }
  bad <- is.na(correlations)
  correlations[!bad]
}

















  corr <- function(directory,threshold=0) {
    all_files <- list.files(directory, full.names=TRUE)
    correlations <- c()
    for(i in 1:length(all_files)){
      data_i <- read.csv(all_files[i])
      nobs <- sum(complete.cases(data_i))
      if( nobs > threshold){
        correlations <- c(correlations,cor(data_i$sulfate,data_i$nitrate))
      }
      else {
        return(numeric())
      }
      return(correlations)
      
    }
  }    
    corr<- function(directory, threshold=0){
      files<-(Sys.glob("specdata//*.csv"))
      correlations<- c()
      for( file in files) {
        file_data<- read.csv(file,sep=",")
        complete_cases<-file_data[complete.cases(file_data), ]
        if(nrow(complete_cases)>threshold){
          correlations<- c(correlations,cor(complete_cases$sulfate, complete_cases$nitrate))
        }
        else {
          return(numeric())
        }
      }
      return(correlations)
    }

  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
}