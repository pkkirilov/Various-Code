set.seed(1)
n <- 1000
null <- vector("numeric", n)
for (i in 1:n){
  control <- sample(data, 5)
  #treatment <- sample(x, 5)
  null[i] <- mean(control) #- mean(treatment)
}


#Set the seed at 1, then using a for-loop take a random sample of 5 mice 1,000 times. Save these averages. 
#What proportion of these 1,000 averages are more than 1 gram away from the average of x ?