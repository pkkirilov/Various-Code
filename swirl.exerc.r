if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl, destfile = "./data/restaurants.csv")
restData <- read.csv("./data/restaurants.csv")
restData$zcf <- factor(restData$zipCode)
class(restData$zcf)

#BEFORE OPENING SWIRL
options(editor = "internal")

