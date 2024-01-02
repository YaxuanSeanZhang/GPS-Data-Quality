#read data-----
all_data <- read.csv("data/_Raw_mpls_all_data_RO.csv",
                     sep=",",header = T)

#create clean_data
clean_Tdata<-all_data[,1:7]
clean_Tdata<-cbind(clean_Tdata,all_data[,58])
clean_Tdata<-cbind(clean_Tdata,all_data[,63:70])
colnames(clean_Tdata)[which(
  colnames(clean_Tdata) == 'all_data[, 58]')] <- 'dis_km'

#read trajectory data-----
actr<-clean_Tdata
actr$route<-as.character(actr$route)
#decode Polyline the route for activity and trip
route<-list()
actrU <- base::split(actr,actr$surveycode)
for(i in 1:length(actrU)){
  route[[i]] <- list()
  names(route)[i] <- names(actrU)[i]
  for (j in 1:length(actrU[[i]]$tripid)) {
    if(is.na(actrU[[i]]$route[j])|actrU[[i]]$route[j]== ''){
      route[[i]]<-append(route[[i]],list(
        data.frame(rbind(c(-999,-999),c(-999,-999)))))
    }
    else{
      route[[i]]<-append(route[[i]],list(
        data.frame(decodePolyline(actrU[[i]]$route[j]))))
    }
    names(route[[i]])[j] <- actrU[[i]][j,'tripid']
  }
}

route_all_record<-route #for back-up

for(i in 1:length(route)){
  for (j in 1:length(route[[i]])) {
    if(length(route[[i]][[j]]$lat)>2){
      #get start point & end point for each trajectory
      route[[i]][[j]]<-route[[i]][[j]][-(2:(length(route[[i]][[j]]$lat)-1)),]   
    }
    else if(length(route[[i]][[j]]$lat)==1){
      #identify single-point trip
      route[[i]][[j]]<-rbind(route[[i]][[j]],route[[i]][[j]])                      
    }
  }
}

actr<-cbind(actr,data.frame(matrix(unlist(route), ncol =4, byrow=T)))

#data formatting-----
#actr<-actr[,-(15:16)] #delete decoded trajectory
colnames(actr)[15:18]<-c("startlat","endlat","startlon","endlon")
colnames(actr)[1:2]<-c("TripId","UserId")
#actr<-actr[,-(7:8)] #delete origin duration & dis_km

##time-----
#time formatting & sorted
actr$starttime<-strptime(as.character(actr$starttime),
                         format = "%Y-%m-%d %H:%M:%S")
actr$endtime<-strptime(as.character(actr$endtime),
                       format = "%Y-%m-%d %H:%M:%S")
actr<-actr[order(actr$UserId,actr$starttime,actr$endtime),]

#check endtime > starttime
n <- length(actr$TripId)
case<-actr$endtime[1:n] > actr$starttime[1:n]   #all true
actr<-actr[which(case == TRUE),]

#add duration (mins)
actr[,"duration"]<-as.numeric(
  difftime(actr[,"endtime"],actr[,"starttime"],units = "mins"))

##subtype----
#format subtype field
actr$subtype <- ifelse(actr$subtype == "HOME", "HO", actr$subtype)
actr$subtype <- ifelse(actr$subtype == "WORK", "WO", actr$subtype)
actr$subtype <- ifelse(actr$subtype == "EDUCATION", "ED", actr$subtype)
actr$subtype <- ifelse(actr$subtype == "PERSONAL_BUSINESS", "PB", actr$subtype)
actr$subtype <- ifelse(actr$subtype == "EAT_OUT", "EO", actr$subtype)
actr$subtype <- ifelse(actr$subtype == "LEISURE_RECREATION", "LR", actr$subtype)
actr$subtype <- ifelse(actr$subtype == "SHOP", "SH", actr$subtype)
actr$subtype <- ifelse(((actr$type == "ACTIVITY") & (actr$subtype == "OTHER")), 
                       "OA", actr$subtype)
actr$subtype <- ifelse(actr$subtype == "WAIT", "WI", actr$subtype)
actr$subtype <- ifelse(actr$subtype == "WALK", "WL", actr$subtype)
actr$subtype <- ifelse(actr$subtype == "BIKE", "BI", actr$subtype)
actr$subtype <- ifelse(actr$subtype == "BUS", "BU", actr$subtype)
actr$subtype <- ifelse(actr$subtype == "RAIL", "RA", actr$subtype)
actr$subtype <- ifelse(actr$subtype == "IN_VEHICLE", "IV", actr$subtype)
actr$subtype <- ifelse(actr$subtype == "CAR", "CA", actr$subtype)
actr$subtype <- ifelse(((actr$type == "TRIP") & (actr$subtype == "OTHER")), 
                       "OT", actr$subtype)

##spatial information------
actr[,'distance'] <- ifelse(
  actr$startlat == -999,
  -999,
  distance84(actr$startlat,actr$startlon,actr$endlat,actr$endlon))

actr[,'speed'] <- ifelse(actr$startlat == -999 ,
                         -999,actr$distance/actr$duration*50/3)  #unit: m/s

##survey----
#assign NA data as 0 for emotion survey
actr[,(7:12)]<-apply(actr[,(7:12)],2,function(x) sapply(x,natransfer)) 

#save file----
write.csv(actr,"data/raw_activity_travel.csv",row.names=FALSE)
