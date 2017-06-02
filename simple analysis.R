#data = read.csv('gifts.csv')

#initialize
install.packages("geosphere")

#
library(geosphere)
data = read.csv('gifts.csv') # location of gifts on globe: latitude, longitude
NorthPole = c(0,90) # sleights start and return with cargo to here
SleightWeight =10 # weight of sleigh adds to score (to be minimized)


#pick each gift on separate sleigh, getting initial higher limit
total=0
for (i in 1:nrow(data)) {
  x = data$Longitude[i]
  y = data$Latitude[i]
  p1= c(x,y)
  #dist = distHaversine(NorthPole, p1, r=6378137)  , meters, need km
  dist = distHaversine(NorthPole, p1, r=6371)  
  total = total +( dist*data$Weight[i])+ 2*dist*SleightWeight
}

# score from kaggle: 29121011015.58950
# ie 29 billion
# leader was 12.3959 billion



#theoretical lower limit, each packets weight on direct route from NorthPole, cant go under this
total=0
for (i in 1:nrow(data)) {
  x = data$Longitude[i]
  y = data$Latitude[i]
  p1= c(x,y)
  dist = distHaversine(NorthPole, p1, r=6371)  
  total = total +( dist*data$Weight[i])
}

# theoretical lower limit:
#12 042 715 199


# format simple solution and write it to csv
solution = data.frame(matrix(0, ncol = 2, nrow = 100000))  
colnames(solution) <- c("GiftId","TripId")
solution$GiftId = data$GiftId
solution$TripId = data$GiftId
write.csv(solution, file = "solution_simple.csv", row.names=FALSE)