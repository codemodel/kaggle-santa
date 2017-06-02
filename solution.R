library(geosphere)
NorthPole = c(0,90)
SleightWeight =10

data = read.csv('gifts.csv')

gifts= data
gifts$SegLong = floor(gifts$Longitude)
gifts$SegLat = floor(gifts$Latitude)

#latitude < -62,5 antarctica
giftsSouth = gifts[gifts$Latitude <= -62.5 ,]
giftsNorth  = gifts[gifts$Latitude > -62.5 ,]

#optimal theoretical solution would be to move each gift directly between NorthPole and location. (0 sidways travel)
#so try to pick gifts that are between furthest chosen gift and NPole. Try to minimize sideways traveling.

#Sort gifts by Longitude. Start adding gifts to trips, always next closest by longitude, until reach 1000 kg. Then new trip.
#Then sort this one trip by Latitude.


gifts = giftsSouth[order(giftsSouth$Longitude),]
gifts["TripId"]=0
#add gifts while <=1000kg from sorted Longitude
trip=1
count=0
weight=0
did=0
for (i in 1:nrow(gifts)) {
  weight=weight+gifts$Weight[i]
  
  count=count+1
   
  if (weight <=1000) {
    gifts$TripId[i]=trip
    
  }
  else {
    gifts$TripId[i]=trip+1
    trip=trip+1
    weight = gifts$Weight[i]
  }
}


# sort by Trip, then closest, ie highest latitude first (drop gift)
new = gifts[with(gifts, order(TripId, -Latitude)), ]

solution = data.frame(matrix(0, ncol = 2, nrow = nrow(new)))
colnames(solution) <- c("GiftId","TripId")
solution$GiftId = new$GiftId
solution$TripId = new$TripId


trip=trip+1

gifts = giftsNorth[order(giftsNorth$Longitude),]
gifts["TripId"]=0
#add gifts while <=1000kg from sorted Longitude
#trip=1
count=0
weight=0
did=0
for (i in 1:nrow(gifts)) {
  weight=weight+gifts$Weight[i]
  
  count=count+1
  
  if (weight <=1000) {
    gifts$TripId[i]=trip
    
  }
  else {
    gifts$TripId[i]=trip+1
    trip=trip+1
    weight = gifts$Weight[i]
  }
}

# sort by Trip, then closest, ie highest latitude first (drop gift)
new = gifts[with(gifts, order(TripId, -Latitude)), ]

solution2 = data.frame(matrix(0, ncol = 2, nrow = nrow(new)))
colnames(solution2) <- c("GiftId","TripId")
solution2$GiftId = new$GiftId
solution2$TripId = new$TripId



solution1 = solution
solution = rbind(solution1, solution2)


write.csv(solution, file = "solution_antarctica.csv", row.names=FALSE)


#Antarctis as separate
# 12 528 250 902.27400
#You improved on your best score by 135 836 600.43537.
#You just moved up 103 positions on the leaderboard.

#score for each gift on separate trip  29121011015.58950     100.000 trips
# add next by latitude to <=1000 kg:  12664087502.70930      1431 trips





#to do, if next gift does not fit to under 1000 kg, check if could take another smaller gift on this trip.

# add check if adding next gift actually is cheaper than starting new trip.


# max weight set to 900 made a bit worse score  12707638156.47540,

ptm <- proc.time()
current = c(gifts$Longitude[i-1], gifts$Latitude[i-1])
x = c(gifts$Longitude[i], gifts$Latitude[i])
dist = distHaversine(current, x, r=6371)  
proc.time() - ptm