################################################
#User Error Rate Summary
################################################
#Data Incompleteness----
actr_orgin =  read.csv("data/raw_activity_travel.csv",
                       sep=",",header = T,stringsAsFactors=FALSE)
na_routeU = base::split(na_route_all,na_route_all$UserId)
miss_count = data.frame(UserId=unique(actr_orgin$UserId), 
                        incomplete=0,temporal=0,spatial=0)
for (i in 1:length(na_routeU)) {
  for (j in 1:nrow(miss_count)) {
    if(names(na_routeU)[i]==miss_count$UserId[j]){
      miss_count$incomplete[j]=nrow(na_routeU[[i]])
    }
  }
}

#Temporal Inconsistency----
temporal = miss_list_all[,c('UserId','TripId','from','to','duration')]

temporalU = base::split(temporal,temporal$UserId)
for (i in 1:length(temporalU)) {
  for (j in 1:nrow(miss_count)) {
    if(names(temporalU)[i]==miss_count$UserId[j]){
      miss_count$temporal[j]=nrow(temporalU[[i]])
    }
  }
}

#Spatial Inconsistency----
spatialdistance_issue = spatialdistance[which(spatialdistance$distance>=0.258),]
spatialU = base::split(spatialdistance_issue,spatialdistance_issue$UserId)
for (i in 1:length(spatialU)) {
  for (j in 1:nrow(miss_count)) {
    if(names(spatialU)[i]==miss_count$UserId[j]){
      miss_count$spatial[j]=nrow(spatialU[[i]])
    }
  }
}


################################################
#Demographic Analysis
################################################
demographic = read.csv("data/Minneapolis, MN SWB Survey Data.csv")
demographic = demographic[,c('SURVEYCODE','GENDER','Q21_1','Q21_2',
                             'Q21_3','Q21_4','Q21_5','Q21_6','Q21_7',
                             'EDUCAT','YRBORN')]
demographic$AGE = 2017-demographic$YRBORN

miss_count = cbind(miss_count,demographic[1,])
miss_count$combined = FALSE

for (i in 1:nrow(miss_count)) {
  for(j in 1:nrow(demographic)){
    if(miss_count$UserId[i]==demographic$SURVEYCODE[j]){
      miss_count[i,6:17] = demographic[j,]
      miss_count$combined[i] = TRUE
    }
  }
}

summary(miss_count$incomplete[which(miss_count$GENDER==0)]/
          miss_count$all[which(miss_count$GENDER==0)])
summary(miss_count$incomplete[which(miss_count$EDUCAT==1)]/
          miss_count$all[which(miss_count$EDUCAT==1)])
summary(miss_count$incomplete[which(miss_count$Q21_1==1)]/
          miss_count$all[which(miss_count$Q21_1==1)])
summary(miss_count$incomplete[which(miss_count$AGE>60)]/
          miss_count$all[which(miss_count$AGE>60)])


################################################
#Evaluation visualization 
################################################
error_label = c('Data Error','Data Loss','Time Unknow Trip',
                'Signal Loss','Signal Loss Trip (from)','Signal Loss Trip (to)')
d = read.csv("data/raw_activity_travel.csv",
             sep=",",header = T,stringsAsFactors=FALSE)

#raw trip data
d1=d[which(d$type=='TRIP'),]
d1$subtype <- factor(d1$subtype , 
                     levels=c('CA','BU','RA','IV','BI','WL','WI','OT'))
#raw activity data
d2=d[which(d$type=='ACTIVITY'),]
d2$subtype <- factor(d2$subtype[which(d2$type=='ACTIVITY')] , 
                     levels=c('HO','WO','ED','PB','EO','LR','SH','OA'))

#cleaned trip data
actr1=actr[which(actr$type=='TRIP'&(actr$label%in%error_label)==FALSE),]
actr1$subtype <- factor(actr1$subtype , 
                        levels=c('CA','BU','RA','IV','BI','WL','WI','OT'))

#cleaned activity data
actr2=actr[which(actr$type=='ACTIVITY'&(actr$label%in%error_label)==FALSE),]
actr2$subtype <- factor(actr2$subtype , 
                        levels=c('HO','WO','ED','PB','EO','LR','SH','OA'))

#boxplot to compare statistical summary of raw and cleaned data
show_trip = rbind(cbind(d1[which(d1$speed>0),c(1:6,13:19)],data='before'),
                  cbind(actr1[which(actr1$subtype!='HS'& actr1$subtype!='LS'),
                              c(1:6,13:19)],data='after'))

p1 <- ggplot(show_trip, aes(x=speed, y=subtype, fill=data)) + 
  geom_boxplot() + coord_flip(xlim=c(0, 15))+ 
  labs(x = 'Speed (m/s)', y='Travel Mode')+
  theme(text = element_text(size=15),legend.text = element_text(size=14))+
  facet_wrap(~subtype, scale="free")
p1

show_activity = rbind(cbind(d2[which(d2$speed>0),c(1:6,13:19)],data='before'),
                      cbind(actr2[which(actr2$subtype!='UA'),
                                  c(1:6,13:19)],data='after'))

p2 <- ggplot(show_activity, aes(x=speed, y=subtype, fill=data)) + 
  geom_boxplot() + coord_flip(xlim=c(0, 0.1))+ 
  labs(x = 'Speed (m/s)', y='Activity Type')+
  theme(text = element_text(size=15),legend.text = element_text(size=14))+
  facet_wrap(~subtype, scale="free")
p2

tapply(d1$speed, d1$subtype, summary)
tapply(actr1$speed, actr1$subtype, summary)

tapply(d2$speed, d2$subtype, summary)
tapply(actr2$speed, actr2$subtype, summary)


################################################
#Spatial visualization 
################################################
m <- leaflet(miss_list_select) %>% 
  addTiles() %>%
  addCircleMarkers(data = miss_list_select,
                   radius = 2,weight = 1,opacity =0.8,fillOpacity = 1,
                   lat = ~startlat, lng = ~startlon) %>%
  addCircleMarkers(data = miss_list_select,
                   radius = 2,weight = 1,opacity =0.8,fillOpacity = 1,
                   lat = ~endlat, lng = ~endlon,color = " ")

for (i in unique(miss_list_select2$TripId)) {
  m <- m %>% 
    addPolylines(data = miss_list_select2[miss_list_select2$TripId == i, ], weight = 1.5,
                 lng = ~lon, 
                 lat = ~lat) 
}

m