################################################
#Define Attribute Redundancy
################################################
actr$same = 0
n <- length(actr$TripId)
for (i in 1:(n-1)) {
  if(actr$UserId[i]==actr$UserId[i+1] & actr$subtype[i]==actr$subtype[i+1] &
     actr$happy[i]==actr$happy[i+1] & actr$sad[i]==actr$sad[i+1] &
     actr$tired[i]==actr$tired[i+1] & actr$stressful[i]==actr$stressful[i+1] &
     actr$pain[i]==actr$pain[i+1] & actr$meaningful[i]==actr$meaningful[i+1]){
    actr$same[i] = 1
  }
}


################################################
#Address Attribute Redundancy
################################################
for (i in 1:nrow(actr)) {
  if(actr$same[i]==1 & actr$same[i+1]==0){
    actr$endtime[i] = actr$endtime[i+1]
    actr$endlat[i] = actr$endlat[i+1]
    actr$endlon[i] = actr$endlon[i+1]
    actr$label[i] = 'SAME'
    route_update[[as.character(actr$UserId[i])]][
      as.character(actr$TripId[i])][[1]] = 
      rbind(route_update[[as.character(actr$UserId[i])]][
        as.character(actr$TripId[i])][[1]],
            route_update[[as.character(actr$UserId[i+1])]][
              as.character(actr$TripId[i+1])][[1]])
    route_update[[as.character(actr$UserId[i+1])]] = 
      route_update[[as.character(actr$UserId[i+1])]][
        names(route_update[[as.character(actr$UserId[i+1])]])!=
          as.character(actr$TripId[i+1])]
    actr$subtype[i+1] = 'REMOVE'
  }
  else if(actr$same[i]==1 & actr$same[i+1]==1){
    actr$endtime[i] = actr$endtime[i+1]
    actr$endlat[i] = actr$endlat[i+1]
    actr$endlon[i] = actr$endlon[i+1]
  }
}

#Update activity-travel data
actr = actr[which(actr$subtype!='REMOVE'),]
#duration
actr[,"duration"]<-difftime(actr[,"endtime"],actr[,"starttime"],units = "mins")
#distance & speed
for (i in 1:length(actr$TripId)) {
  if(actr$startlat[i]!=-999 & actr$label[i]!='Data Error'& 
     actr$label[i]!='Data Loss'){
    actr$distance[i] = distance84(actr[i,'endlat'],actr[i,'endlon'],
                                  actr[i,'startlat'],actr[i,'startlon'])
    actr$speed[i] = actr$distance[i]/as.numeric(actr$duration[i])*50/3
  }
}