#data = read.csv('gifts.csv')

#pick each gift on separate sleigh

library(geosphere)
NorthPole = c(0,90)
SleightWeight =10

solution = data.frame(matrix(0, ncol = 2, nrow = 100000))  
colnames(solution) <- c("GiftId","TripId")

  
solution$GiftId = data$GiftId
solution$TripId = data$GiftId

write.csv(solution, file = "solution_simple.csv", row.names=FALSE)

# score from kaggle: 29121011015.58950
# # ie 29 
#leader was 123959.. billion


# theoretical lower limit:  1.205621e+13 (not indlucing sleigh weight)
12 042 715 199
#calculate score

total=0
for (i in 1:nrow(data)) {
  x = data$Longitude[i]
  y = data$Latitude[i]
  p1= c(x,y)
  #dist = distHaversine(NorthPole, p1, r=6378137)  , meters, need km
  dist = distHaversine(NorthPole, p1, r=6371)  
  total = total +( dist*data$Weight[i])+ 2*dist*SleightWeight
  }


#theoretical lower limit, each packets weight on direct route from NorthPole, cant go under this
# 12 042 715 199

total=0
for (i in 1:nrow(data)) {
  x = data$Longitude[i]
  y = data$Latitude[i]
  p1= c(x,y)
  dist = distHaversine(NorthPole, p1, r=6371)  
  total = total +( dist*data$Weight[i])
}