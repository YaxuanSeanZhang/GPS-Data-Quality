#Self-defined Functions-----
get_na_route = function(actr){
  actr_missing<-actr[which(actr$startlon==-999),]
  #get the missing route data
  n<-length(actr$TripId)
  #route of first record is missing
  case_first<-(actr$startlon[2:(n-1)]==-999) & 
    (actr$UserId[2:(n - 1)] != actr$UserId[1:(n-2)]) & 
    (actr$UserId[2:(n - 1)] == actr$UserId[3:n])
  #route of last record is missing
  case_last<- (actr$startlon[2:(n-1)]==-999) & 
    (actr$UserId[2:(n - 1)] == actr$UserId[1:(n-2)]) & 
    (actr$UserId[2:(n - 1)] != actr$UserId[3:n])  
  
  na_route<-actr_missing
  colnames(na_route)[13:16]<-c("formerlat","formerlon","laterlat","laterlon")
  j = 0
  #get missing route and its previous and following record information
  for (i in 1:length(actr$TripId)) {     
    if(actr$startlon[i]==-999){
      j = j+1
      if (case_first[i-1] | case_last[i-1]){
        na_route$subtype[j] = 'REMOVE'
        actr$subtype[i] = 'REMOVE'
      }
      else{
        na_route$former[j] = as.character(actr$subtype[i-1])
        na_route$later[j] = as.character(actr$subtype[i+1])
        na_route$formerlat[j] = actr$endlat[i-1]
        na_route$formerlon[j] = actr$endlon[i-1]
        na_route$laterlat[j] = actr$startlat[i+1]
        na_route$laterlon[j] = actr$startlon[i+1]
        na_route$formerduration[j] = actr$duration[i-1]
        na_route$laterduration[j] = actr$duration[i+1]
        na_route$formerspeed[j] = actr$speed[i-1]
        na_route$laterspeed[j] = actr$speed[i+1]
      }
    }
  }
  return(na_route)
}



################################################
#Define Incomplete Data (Missing Trajectory)
################################################
na_route = get_na_route(actr)

#delete the first and last record if its route information is missing
temp = na_route[which(na_route$subtype=='REMOVE'),]
for(i in 2:(length(actr$TripId)-1)){
  for (j in 1:length(temp$TripId)) {
    if(actr$TripId[i] == temp$TripId[j]){
      actr$subtype[i] = 'REMOVE' 
    }
  }
}

#update route table
na_route<-na_route[which(na_route$subtype!='REMOVE'),]
remove = data.frame(UserId=actr[which(actr$subtype== 'REMOVE'),'UserId'],
                    TripId=actr[which(actr$subtype== 'REMOVE'),'TripId'])

route_update = route_all_record

for (i in 1:length(remove$UserId)) {
  route_update[[as.character(remove$UserId[i])]] = 
    route_update[[as.character(remove$UserId[i])]][
      names(route_update[[
        as.character(remove$UserId[i])]])!=as.character(remove$TripId[i])]
}

#update activity-travel table
actr<-actr[which(actr$subtype!= 'REMOVE'),]



################################################
#Interpolate Missing Trajectory
################################################
#Step1: Temporal: duration classification----
#pick the best fitted model based on BIC value for classification

#visual exploration[optional]
na_route = na_route[which(na_route$speed!=-999 & 
                            na_route$special!=1 & na_route$special!=2),]
na_route$duration = as.numeric(na_route$duration)
table(na_route$duration)


r_hist <- hist(na_route$duration, breaks = seq(0, max(na_route$duration), by=1))
inXr = r_hist$mids + 0.5
inYr = r_hist$density
fit_datar <- data.frame(inXr, inYr)
hist(na_route$duration,xlim=c(0,60),breaks = seq(0, max(inXr), by=1),prob=T,
     xlab="minute (1-60min)",ylab='prob',
     main = "Histogram of missing route durations")

#mixture model fitting
##gaussian
mmNO1r <- gamlssMXfits(n =5, na_route$duration~1, family=NO, K=1) 
mmNO2r <- gamlssMXfits(n =5, na_route$duration~1, family=NO, K=2) #fail
mmNO3r <- gamlssMXfits(n =5, na_route$duration~1, family=NO, K=3) #fail

##inverse gaussian
mmIG1r <- gamlssMXfits(n =5, na_route$duration~1, family=IG, K=1)
mmIG2r <- gamlssMXfits(n =5, na_route$duration~1, family=IG, K=2) #fail
mmIG3r <- gamlssMXfits(n =5, na_route$duration~1, family=IG, K=3) #fail

##generalized inverse gaussian
mmGIG1r <- gamlssMXfits(n =5, na_route$duration~1, family=GIG, K=1)
mmGIG2r <- gamlssMXfits(n =5, na_route$duration~1, family=GIG, K=2)
mmGIG3r <- gamlssMXfits(n =5, na_route$duration~1, family=GIG, K=3)

##log-normall
mmLOGNO1r <- gamlssMXfits(n =5, na_route$duration~1, family=LOGNO2, K=1)
mmLOGNO2r <- gamlssMXfits(n =5, na_route$duration~1, family=LOGNO2, K=2) #fail
mmLOGNO3r <- gamlssMXfits(n =5, na_route$duration~1, family=LOGNO2, K=3) #fail

##inverse gamma
mmIGAMMA1r <- gamlssMXfits(n =5, na_route$duration~1, family=IGAMMA, K=1)
mmIGAMMA2r <- gamlssMXfits(n =5, na_route$duration~1, family=IGAMMA, K=2) #fail
mmIGAMMA3r <- gamlssMXfits(n =5, na_route$duration~1, family=IGAMMA, K=3) #fail

##reverse gumbel
mmRG1r <- gamlssMXfits(n =5, na_route$duration~1, family=RG, K=1)
mmRG2r <- gamlssMXfits(n =5, na_route$duration~1, family=RG, K=2) #fail
mmRG3r <- gamlssMXfits(n =5, na_route$duration~1, family=RG, K=3) #fail

##exponential
mmEXP1r <- gamlssMXfits(n =5, na_route$duration~1, family=EXP, K=1)
mmEXP2r <- gamlssMXfits(n =5, na_route$duration~1, family=EXP, K=2) #best
mmEXP3r <- gamlssMXfits(n =5, na_route$duration~1, family=EXP, K=3)


#BIC plot for evaluation
BIC_r = data.frame(NA,NA,NA,NA,NA,NA)

BIC_r[1,] = c(mmIG1r$sbc,mmIGAMMA1r$sbc,mmRG1r$sbc,
              mmEXP1r$sbc,mmLOGNO1r$sbc,mmNO1r$sbc)
BIC_r[2,] = c(NA,NA,NA,mmEXP2r$sbc,NA,NA)
BIC_r[3,] = c(NA,NA,NA,mmEXP3r$sbc,NA,NA)
BIC_r = cbind(c(1,2,3),BIC_r)

colnames(BIC_r) = c('num_of_component','Inverse_Gaussian', 
                    'Inverse_Gamma', 'Reverse_Gumbel', 
                    'Exponential', 'Log_Normal', 'Gaussian')
#BIC plot for paper
ggplot(data=BIC_r) +
  geom_point(aes(x=as.factor(num_of_component), y=Inverse_Gaussian),
             shape = 15,size=3,color = 'forestgreen') + 
  geom_point(aes(x=as.factor(num_of_component), y=Inverse_Gamma),
             shape = 11,size=3,fill = 'indianred3',color = 'indianred3') + 
  geom_point(aes(x=as.factor(num_of_component), y=Reverse_Gumbel),
             shape = 17,size=3,color = 'forestgreen') + 
  geom_point(aes(x=as.factor(num_of_component), y=Exponential),
             shape = 25,size=2.7,fill = 'indianred3',color = 'indianred3') + 
  geom_point(aes(x=as.factor(num_of_component), y=Log_Normal),
             shape = 23,size=3,fill = 'deepskyblue3',color = 'deepskyblue3') + 
  geom_point(aes(x=as.factor(num_of_component), y=Gaussian),
             shape = 16,size=3,color = 'deepskyblue3') + 
  geom_line(aes(x=as.factor(num_of_component), y=Exponential,group = 1),
            color = 'indianred3',size = 1) +
  labs(x = "Number of Compoents",y = 'BIC') + 
  theme(text = element_text(size=15)) +
  geom_text(aes(x=as.factor(num_of_component), 
                y=Inverse_Gaussian,
                label=colnames(BIC_r)[2]),
            hjust=-0.1, vjust=0.6,color = 'forestgreen' ,size=4.75) +
  geom_text(data=subset(BIC_r, num_of_component == 1),
            aes(x=as.factor(num_of_component), 
                y=Inverse_Gamma,
                label=colnames(BIC_r)[3]),
            hjust=-0.1, vjust=0,color = 'indianred3' ,size=4.75) +
  geom_text(data=subset(BIC_r, num_of_component == 1),
            aes(x=as.factor(num_of_component), 
                y=Reverse_Gumbel,
                label=colnames(BIC_r)[4]),
            hjust=-0.1, vjust=0,color = 'forestgreen' ,size=4.75) +
  geom_text(data=subset(BIC_r, num_of_component == 1),
            aes(x=as.factor(num_of_component), 
                y=Exponential,
                label=colnames(BIC_r)[5]),
            hjust=-0.1, vjust=0,color = 'indianred3' ,size=4.75) +
  geom_text(data=subset(BIC_r, num_of_component == 1),
            aes(x=as.factor(num_of_component), 
                y=Log_Normal,
                label=colnames(BIC_r)[6]),
            hjust=-0.13, vjust=-0.2,color = 'deepskyblue3' ,size=4.75) +
  geom_text(data=subset(BIC_r, num_of_component == 1),
            aes(x=as.factor(num_of_component), 
                y=Gaussian,
                label=colnames(BIC_r)[7]),
            hjust=-0.1, vjust=0,color = 'deepskyblue3',size=4.75)

###best fit: 1 component inverse GAMMA

fitr = data.frame(duration = fit_datar$inXr, 
                  prob = dIGAMMA(fit_datar$inXr, 
                                 mu=exp(-0.259), sigma=exp(-0.3242)))
#classification result for paper
ggplot(na_route, aes(x=duration)) + 
  geom_histogram(aes(y=..density..), position="identity",binwidth = 1) + 
  xlim(c(0, 60)) + labs(x = "incompleteness duration (1-60min)",y = 'prob') + 
  theme(text = element_text(size=15)) +
  geom_line(data = fitr,aes(x=duration, y=prob),color = 'red',size = 1) 

#Step2: Spatio-temporal: distance-speed clustering----
#pick the best clustering method based on gap_stat value for classification

#normalization
na_route$distance_sd = (na_route$distance-min(na_route$distance))/
  (max(na_route$distance)-min(na_route$distance))
na_route$speed_sd = (na_route$speed-min(na_route$speed))/
  (max(na_route$speed)-min(na_route$speed))

####method family 1: hierarchical clustering (best)----
x = as.matrix(na_route[,c('distance_sd','speed_sd')])
d <- dist(as.matrix(x))
hclust4 <- hclust(d, method = "ward.D")

wss = get_nbclust(x, FUN = hcut, method = "wss",print.summary=TRUE,k.max = 8)
gap_stat <- clusGap(x, FUN = hcut,  K.max = 8, B = 50)

hcluster_r = data.frame(cbind(num=1:8,wss = wss$y, gap = gap_stat$Tab))
colnames(hcluster_r)[6] = 'SEsim'

#gap_stat plot to decide optimal numer of clusters
ggplot(data = hcluster_r,aes(x = num))+
  geom_line(aes(y=wss,color = 'WSS'),) +
  geom_point(aes(y=wss),color = 'steelblue') +
  geom_line(aes(y=gap*2,color = 'Gap Stat'),) +
  geom_point(aes(y=gap*2),color = 'indianred3') +
  geom_errorbar(aes(y=gap*2,ymin=(gap-SEsim)*2, ymax=(gap+SEsim)*2), 
                colour="indianred3", width=.1) +
  scale_y_continuous(name = 'Total Within Sum of Square',
                     sec.axis = sec_axis(~.*0.5, name = "Gap Statistics")) +
  scale_colour_manual(values = c("indianred3","steelblue")) +
  labs(y = "Total Within Sum of Square",x = "Number of clusters",
       colour = "Creterion")+
  theme(legend.position = c(0.87, 0.5),text = element_text(size=15),
        legend.text = element_text(size=14))+
  geom_vline(xintercept = 5, linetype = 2,color = 'steelblue')


sub_grp <- cutree(hclust4, k = 5)
na_route_hcut = cbind(na_route,sub_grp) #get cluster data.frame
colnames(na_route_hcut)[length(na_route_hcut)] = 'group'
na_route_hcut$group=as.character(na_route_hcut$group)

cols    <- c( "1" = "indianred3", "2" = "springgreen4",
              "3" = "goldenrod3", "4" = "deepskyblue3","5" = "deeppink3")

#clustering results
ggplot(data=na_route_hcut, aes(x=distance, y=speed,color=group)) + 
  geom_point()+
  labs(x = "distance (km)",y = 'speed (m/s)') + 
  theme(text = element_text(size=15),legend.text = element_text(size=14))+
  scale_color_manual(name = "group", breaks = c('1','2','3','4','5'), 
                     values = cols,labels = c('1','2','3','4','5'))

####method family 2: k-means clustering ------
wss = get_nbclust(x, FUN = kmeans, method = "wss",print.summary=TRUE,k.max = 8)
gap_stat <- get_nbclust(x, FUN = kmeans, method = "gap_stat",
                        print.summary=TRUE,k.max = 8)

kcluster_r = data.frame(cbind(num=1:8,wss = wss$y, gap = gap_stat$Tab))
colnames(kcluster_r)[6] = 'SEsim'

#gap_stat plot to decide optimal numer of clusters
ggplot(data = kcluster_r,aes(x = num))+
  geom_line(aes(y=wss,color = 'WSS'),) +
  geom_point(aes(y=wss),color = 'steelblue') +
  geom_line(aes(y=gap*2,color = 'Gap Stat'),) +
  geom_point(aes(y=gap*2),color = 'indianred3') +
  geom_errorbar(aes(y=gap*2,ymin=(gap-SEsim)*2, ymax=(gap+SEsim)*2), 
                colour="indianred3", width=.1) +
  scale_y_continuous(name = 'Total Within Sum of Square',
                     sec.axis = sec_axis(~.*0.5, name = "Gap Statistics")) +
  scale_colour_manual(values = c("indianred3","steelblue")) +
  labs(y = "Total Within Sum of Square",
       x = "Number of clusters",
       colour = "Creterion")+
  theme(legend.position = c(0.87, 0.5),text = element_text(size=15),
        legend.text = element_text(size=14))+
  geom_vline(xintercept = 5, linetype = 2,color = 'steelblue')

set.seed(123)
a.res <- kmeans(x,5)
na_route_cluster = cbind(na_route,a.res$cluster) #get cluster data.frame
colnames(na_route_cluster)[length(na_route_cluster)] = 'group'
na_route_cluster$group=as.character(na_route_cluster$group)

cols    <- c( "1" = "indianred3", "2" = "springgreen4",
              "3" = "goldenrod3", "4" = "deepskyblue3","5" = "deeppink3")

#clustering results
ggplot(data=na_route_cluster, aes(x=distance, y=speed,color=group)) + 
  geom_point() + 
  labs(x = "distance (km)",y = 'speed (m/s)') + 
  theme(text = element_text(size=15),legend.text = element_text(size=14))+
  scale_color_manual(name = "group", breaks = c('1','2','3','4','5'), 
                     values = cols,labels = c('1','2','3','4','5'))

####method family 3: fuzzy cluster ------
wss = get_nbclust(x, FUN = cluster::fanny, method = "wss",
                  print.summary=TRUE,k.max = 8)
gap_stat <- get_nbclust(x, FUN = cluster::fanny, method = "gap_stat",
                        print.summary=TRUE,k.max = 8)

fcluster_r = data.frame(cbind(num=1:8,wss = wss$y, gap = gap_stat$Tab))
colnames(fcluster_r)[6] = 'SEsim'

#gap_stat plot to decide optimal numer of clusters
ggplot(data = fcluster_r,aes(x = num))+
  geom_line(aes(y=wss,color = 'WSS'),) +
  geom_point(aes(y=wss),color = 'steelblue') +
  geom_line(aes(y=gap*2,color = 'Gap Stat'),) +
  geom_point(aes(y=gap*2),color = 'indianred3') +
  geom_errorbar(aes(y=gap*2,ymin=(gap-SEsim)*2, ymax=(gap+SEsim)*2), 
                colour="indianred3", width=.1) +
  scale_y_continuous(name = 'Total Within Sum of Square', limits =c(0,4.5),
                     sec.axis = sec_axis(~.*1/2, name = "Gap Statistics")) +
  scale_colour_manual(values = c("indianred3","steelblue")) +
  labs(y = "Total Within Sum of Square",
       x = "Number of clusters",
       colour = "Creterion")+
  theme(legend.position = c(0.8, 0.85),text = element_text(size=15),
        legend.text = element_text(size=14))+
  geom_vline(xintercept = 4, linetype = 2,color = 'steelblue')

fannyx <- fanny(x, 4,stand=FALSE)
na_route_fanny = cbind(na_route,fannyx$clustering) #get cluster data.frame
colnames(na_route_fanny)[length(na_route_fanny)] = 'group'
na_route_fanny$group=as.character(na_route_fanny$group)

cols    <- c( "1" = "indianred3", "2" = "springgreen4",
              "3" = "goldenrod3", "4" = "deepskyblue3")

#clustering results
ggplot(data=na_route_fanny, aes(x=distance, y=speed,color=group)) + 
  geom_point() + 
  labs(x = "distance (km)",y = 'speed (m/s)') + 
  theme(text = element_text(size=15),legend.text = element_text(size=14))+
  scale_color_manual(name = "group", breaks = c('1','2','3','4'), 
                     values = cols,labels = c('1','2','3','4'))

#Step3: interpolation for each ST group------
#by using exploratory analysis and domain knowledge, decide how to interpolate

##ST group 1
na_route_1 = na_route_hcut[which(na_route_hcut$group==1),]

na_route_1$subtype <- factor(na_route_1$subtype , 
                             levels=c("CA", "IV", "BU", "BI",'WL','WI','OT',
                                      'HO','PB','SH','EO','LR'))

ggplot(na_route_1[which(na_route_1$type=='ACTIVITY'),], 
       aes(x=subtype, y=speed, fill=subtype)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") + 
  theme(text = element_text(size=15)) +
  labs(x = "Activity Type",y = 'Speed (m/s)')

ggplot(na_route_1[which(na_route_1$type=='TRIP'),], 
       aes(x=subtype, y=speed, fill=subtype)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") + 
  theme(text = element_text(size=15)) +
  labs(x = "Travel Mode",y = 'Speed (m/s)') 

temp = na_route_1[which(na_route_1$subtype!='WI'& na_route_1$subtype!='PB'),]
for (i in 1:length(temp$TripId)) {
  for (j in 1:length(actr$TripId)) {
    if(!is.na(actr$TripId[j])){
      if(temp$TripId[i]==actr$TripId[j]){
        actr[j,'label'] = 'NO ROUTE'
        actr[j,c('startlat','startlon','endlat','endlon')] = 
          temp[i,c('formerlat','formerlon','laterlat','laterlon')]
        
        route_update[[as.character(actr$UserId[j])]][
          as.character(actr$TripId[j])][[1]][1,] = 
          actr[j,c('startlat','startlon')]
        route_update[[as.character(actr$UserId[j])]][
          as.character(actr$TripId[j])][[1]][2,] = 
          actr[j,c('endlat','endlon')]
        names(route_update[[as.character(actr$UserId[j])]][
          as.character(actr$TripId[j])][[1]]) = c('lat','lon')
      }
    }
  }
}

##ST group2
na_route_2 = na_route_hcut[which(na_route_hcut$group==2),]

na_route_2$subtype <- factor(na_route_2$subtype , 
                             levels=c("CA", "IV", "BU", "BI",'WL','WI'))

ggplot(na_route_2, aes(x=subtype, y=speed, fill=subtype)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") + 
  labs(x = "Activity Type/ Travel Mode",y = 'speed (m/s)') 


temp = na_route_2[which(na_route_2$subtype!='WL' & na_route_2$subtype!='WI'),]
for (i in 1:length(temp$TripId)) {
  for (j in 1:length(actr$TripId)) {
    if(!is.na(actr$TripId[j])){
      if(temp$TripId[i]==actr$TripId[j]){
        actr[j,'label'] = 'NO ROUTE'
        actr[j,c('startlat','startlon','endlat','endlon')] = 
          temp[i,c('formerlat','formerlon','laterlat','laterlon')]
        
        route_update[[as.character(actr$UserId[j])]][
          as.character(actr$TripId[j])][[1]][1,] = 
          actr[j,c('startlat','startlon')]
        route_update[[as.character(actr$UserId[j])]][
          as.character(actr$TripId[j])][[1]][2,] = 
          actr[j,c('endlat','endlon')]
        names(route_update[[as.character(actr$UserId[j])]][
          as.character(actr$TripId[j])][[1]]) = c('lat','lon')
        
      }
    }
  }
}

#ST group 3 & 4
na_route_3 = na_route_hcut[which(na_route_hcut$group==3),]
na_route_4 = na_route_hcut[which(na_route_hcut$group==4),]
temp = rbind(na_route_3[which(na_route_3$subtype!='WI'),],na_route_4)
for (i in 1:length(temp$TripId)) {
  for (j in 1:length(actr$TripId)) {
    if(!is.na(actr$TripId[j])){
      if(temp$TripId[i]==actr$TripId[j]){
        actr[j,'label'] = 'NO ROUTE'
        actr[j,c('startlat','startlon','endlat','endlon')] = 
          temp[i,c('formerlat','formerlon','laterlat','laterlon')]
        
        route_update[[as.character(actr$UserId[j])]][
          as.character(actr$TripId[j])][[1]][1,] = 
          actr[j,c('startlat','startlon')]
        route_update[[as.character(actr$UserId[j])]][
          as.character(actr$TripId[j])][[1]][2,] = 
          actr[j,c('endlat','endlon')]
        names(route_update[[as.character(actr$UserId[j])]][
          as.character(actr$TripId[j])][[1]]) = c('lat','lon')
        
      }
    }
  }
}
