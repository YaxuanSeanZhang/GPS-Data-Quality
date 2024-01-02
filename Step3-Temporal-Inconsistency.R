################################################
#Define Temporal Inconsistency
################################################
#temporal inconsistency - gap ------
n <- length(actr$TripId)
case1 <- (actr$endtime[1:(n - 1)] < actr$starttime[2:n]) & 
  (actr$UserId[1:(n - 1)] == actr$UserId[2:n])

#fill in the gap for later interpolation
miss <- actr[1:(n-1),]
miss$subtype[1:(n-1)]<-ifelse(case1[1:(n-1)] == TRUE,"MI","REMOVE")
miss$starttime[1:(n-1)]<-ifelse(case1[1:(n-1)] == TRUE,
                                as.character(actr$endtime[1:(n-1)]),NA)
miss$endtime[1:(n-1)]<-ifelse(case1[1:(n-1)] == TRUE,
                              as.character(actr$starttime[2:n]),NA)
miss$TripId<-paste(miss$TripId,"MI",sep = '')
miss<-miss[-(which(miss$subtype=="REMOVE")),]
miss$type<-"ACTIVITY"
miss[,7:19]<-0
actr<-rbind(actr,miss)
actr[,"duration"]<-difftime(actr[,"endtime"],actr[,"starttime"],units = "mins") 
actr<-actr[order(actr$UserId,actr$starttime,actr$endtime),] 

#temporal inconsistency - overlap ------
n <- length(actr$TripId)
case2 <- (actr$endtime[1:(n - 1)] > actr$starttime[2:n]) & 
  (actr$endtime[1:(n - 1)] < actr$endtime[2:n]) & 
  (actr$starttime[1:(n - 1)] < actr$starttime[2:n]) & 
  (actr$UserId[1:(n - 1)] == actr$UserId[2:n])

#temporal inconsistency - within ------
n <- length(actr$TripId)
case3 <- (actr$endtime[1:(n - 1)] > actr$starttime[2:n]) & 
  ((actr$endtime[1:(n-1)] >= actr$endtime[2:n])| 
     (actr$starttime[1:(n - 1)] == actr$starttime[2:n]))  & 
  (actr$UserId[1:(n - 1)] == actr$UserId[2:n])

#label spatial cases for further look (manually adjustable)-----
#some bundled inconsistency issues (very rare), e.g.:
###missing route + temporal gap (label as special = 1)
###missing route + temporal within (label as special = 2)
###missing route + attribute redundancy (label as special = 3)

################################################
#Interpolate Temporal Gaps
################################################
#Step0: Create temporal gaps-----
miss_list<-data.frame(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
colnames(miss_list)<-c('UserId',"from","to","duration","starttime","endtime",
                       "startlat","endlat","startlon","endlon","TripId",
                       "formerspeed",'laterspeed','same','special')
j = 1
for (i in 1:(length(actr$TripId)-2)) {
  if(actr$subtype[i+1] == "MI"){
    miss_list$UserId[j] = as.character(actr$UserId[i])
    miss_list$from[j] = as.character(actr$subtype[i])
    miss_list$to[j] = as.character(actr$subtype[i+2])
    miss_list$formerspeed[j] = actr$speed[i]
    miss_list$laterspeed[j] = actr$speed[i+2]
    miss_list$duration[j] = actr$duration[i+1]
    miss_list$starttime[j] = as.character(actr$starttime[i+1])
    miss_list$endtime[j] = as.character(actr$endtime[i+1])
    miss_list$TripId[j] = actr$TripId[i]
    miss_list$same[j] = actr$same[i]
    miss_list$special[j] = actr$special[i+1]
    if(actr$special[i+1]==0){
      miss_list$startlat[j] = actr$endlat[i]
      miss_list$startlon[j] = actr$endlon[i]
      miss_list$endlat[j] = actr$startlat[i+2]
      miss_list$endlon[j] = actr$startlon[i+2]
    }
    else if(actr$special[i+1]==1 & actr$startlat[i+2]==-999){
      miss_list$startlat[j] = actr$endlat[i]
      miss_list$startlon[j] = actr$endlon[i]
      miss_list$endlat[j] = actr$startlat[i+3]
      miss_list$endlon[j] = actr$startlon[i+3]
    }
    else if(actr$special[i+1]==1 & actr$startlat[i]==-999){
      miss_list$startlat[j] = actr$endlat[i-1]
      miss_list$startlon[j] = actr$endlon[i-1]
      miss_list$endlat[j] = actr$startlat[i+2]
      miss_list$endlon[j] = actr$startlon[i+2]
    }
    else if(actr$special[i+1]==2){
      miss_list$startlat[j] = actr$endlat[i-1]
      miss_list$startlon[j] = actr$endlon[i-1]
      miss_list$endlat[j] = actr$startlat[i+2]
      miss_list$endlon[j] = actr$startlon[i+2]
    }
    miss_list <- rbind(miss_list,NA)
    j = j+1
  }
}
miss_list<-na.omit(miss_list)
miss_list_all = miss_list
miss_list = miss_list[which(miss_list$duration<1440),]

#create activity-travel transition state
activity_list<-c('HO','WO','ED','PB','EO','LR','SH','OA')
trip_list<-c('BU','RA','CA','IV','BI','WL','WI','OT')
miss_list<-cbind(miss_list,NA)
colnames(miss_list)[length(miss_list)]<-"case"

##activity to activity
miss_list$case[which((miss_list$from %in% activity_list) & 
                       (miss_list$to %in% activity_list))]<- 1
##trip to activity
miss_list$case[which((miss_list$from %in% trip_list) & 
                       (miss_list$to %in% activity_list))]<- 2
##activity to trip
miss_list$case[which((miss_list$from %in% activity_list) & 
                       (miss_list$to %in% trip_list))]<- 3
##trip to trip
miss_list$case[which((miss_list$from %in% trip_list) & 
                       (miss_list$to %in% trip_list))]<- 4

#impute distance & speed, based on previous and/or following episode records
miss_list[ ,"distance"] <- NA
miss_list[,"speed"]<-NA
for (i in 1:length(miss_list$UserId)) {
  if(miss_list$startlat[i]!=-999 & miss_list$endlat[i]!= -999){
    miss_list$distance[i] <- distance84(
      miss_list$startlat[i],miss_list$startlon[i],
      miss_list$endlat[i],miss_list$endlon[i])
    miss_list$speed[i] <- miss_list$distance[i]/miss_list$duration[i]*50/3  
  }
  else{
    miss_list$distance = -999
    miss_list$speed = -999
  }
}
summary(miss_list$duration)

#exclude record with unrealistic speed
miss_list = miss_list[which(miss_list$speed<90),]


#Step1: Temporal: duration classification----
#pick the best fitted model based on BIC value for classification

#visual exploration[optional]
g_hist <- hist(miss_list$duration, breaks = seq(0, max(miss_list$duration), by=1))
inXg = g_hist$mids + 0.5
inYg = g_hist$density
fit_datag <- data.frame(inXg, inYg)
hist(miss_list$duration,xlim=c(0,60),breaks = seq(0, max(inXg), by=1),prob=T,
     xlab="minute (1-60min)",ylab='prob',
     main = "Histogram of time gap durations")

#mixture model fitting
##gaussian
mmNO1g <- gamlssMXfits(n =5, miss_list$duration~1, family=NO, K=1)
mmNO2g <- gamlssMXfits(n =5, miss_list$duration~1, family=NO, K=2)
mmNO3g <- gamlssMXfits(n =5, miss_list$duration~1, family=NO, K=3) #best
mmNO4g <- gamlssMXfits(n =5, miss_list$duration~1, family=NO, K=4) #not found
mmNO5g <- gamlssMXfits(n =5, miss_list$duration~1, family=NO, K=5)
mmNO6g <- gamlssMXfits(n =5, miss_list$duration~1, family=NO, K=6)
mmNO7g <- gamlssMXfits(n =5, miss_list$duration~1, family=NO, K=7)

##inverse gaussian
mmIG1g <- gamlssMXfits(n =5, miss_list$duration~1, family=IG, K=1)
mmIG2g <- gamlssMXfits(n =5, miss_list$duration~1, family=IG, K=2)
mmIG3g <- gamlssMXfits(n =5, miss_list$duration~1, family=IG, K=3) # best
mmIG4g <- gamlssMXfits(n =5, miss_list$duration~1, family=IG, K=4)
mmIG5g <- gamlssMXfits(n =5, miss_list$duration~1, family=IG, K=5)
mmIG6g <- gamlssMXfits(n =5, miss_list$duration~1, family=IG, K=6)
mmIG7g <- gamlssMXfits(n =5, miss_list$duration~1, family=IG, K=7)

##log-normall
mmLOGNO1g <- gamlssMXfits(n =5, miss_list$duration~1, family=LOGNO2, K=1)
mmLOGNO2g <- gamlssMXfits(n =5, miss_list$duration~1, family=LOGNO2, K=2)
mmLOGNO3g <- gamlssMXfits(n =5, miss_list$duration~1, family=LOGNO2, K=3) #best
mmLOGNO4g <- gamlssMXfits(n =5, miss_list$duration~1, family=LOGNO2, K=4) #not found
mmLOGNO5g <- gamlssMXfits(n =5, miss_list$duration~1, family=LOGNO2, K=5)
mmLOGNO6g <- gamlssMXfits(n =5, miss_list$duration~1, family=LOGNO2, K=6)

##inverse gamma
mmIGAMMA1g <- gamlssMXfits(n =5, miss_list$duration~1, family=IGAMMA, K=1) #best
mmIGAMMA2g <- gamlssMXfits(n =5, miss_list$duration~1, family=IGAMMA, K=2)
mmIGAMMA3g <- gamlssMXfits(n =5, miss_list$duration~1, family=IGAMMA, K=3)
mmIGAMMA4g <- gamlssMXfits(n =5, miss_list$duration~1, family=IGAMMA, K=4)
mmIGAMMA5g <- gamlssMXfits(n =5, miss_list$duration~1, family=IGAMMA, K=5)
mmIGAMMA6g <- gamlssMXfits(n =5, miss_list$duration~1, family=IGAMMA, K=6)
mmIGAMMA7g <- gamlssMXfits(n =5, miss_list$duration~1, family=IGAMMA, K=7)


##reverse gumbel
mmRG1g <- gamlssMXfits(n =5, miss_list$duration~1, family=RG, K=1)
mmRG2g <- gamlssMXfits(n =5, miss_list$duration~1, family=RG, K=2)
mmRG3g <- gamlssMXfits(n =5, miss_list$duration~1, family=RG, K=3)
mmRG4g <- gamlssMXfits(n =5, miss_list$duration~1, family=RG, K=4)
mmRG5g <- gamlssMXfits(n =5, miss_list$duration~1, family=RG, K=5)
mmRG6g <- gamlssMXfits(n =5, miss_list$duration~1, family=RG, K=6) #best
mmRG7g <- gamlssMXfits(n =5, miss_list$duration~1, family=RG, K=7)

##exponent
mmEXP1g <- gamlssMXfits(n =5, miss_list$duration~1, family=EXP, K=1)
mmEXP2g <- gamlssMXfits(n =5, miss_list$duration~1, family=EXP, K=2) #best
mmEXP3g <- gamlssMXfits(n =5, miss_list$duration~1, family=EXP, K=3)
mmEXP4g <- gamlssMXfits(n =5, miss_list$duration~1, family=EXP, K=4)
mmEXP5g <- gamlssMXfits(n =5, miss_list$duration~1, family=EXP, K=5)
mmEXP6g <- gamlssMXfits(n =5, miss_list$duration~1, family=EXP, K=6)
mmEXP7g <- gamlssMXfits(n =5, miss_list$duration~1, family=EXP, K=7)

#BIC plot for evaluation
BIC_g = data.frame(NA,NA,NA,NA,NA,NA)

BIC_g[1,] = c(mmIG1g$sbc,mmIGAMMA1g$sbc,mmRG1g$sbc,mmEXP1g$sbc,
              mmLOGNO1g$sbc,mmNO1g$sbc)
BIC_g[2,] = c(mmIG2g$sbc,mmIGAMMA2g$sbc,mmRG2g$sbc,mmEXP2g$sbc,
              mmLOGNO2g$sbc,mmNO2g$sbc)
BIC_g[3,] = c(mmIG3g$sbc,mmIGAMMA3g$sbc,mmRG3g$sbc,mmEXP3g$sbc,
              mmLOGNO3g$sbc,mmNO3g$sbc)
BIC_g[4,] = c(mmIG4g$sbc,mmIGAMMA4g$sbc,mmRG4g$sbc,mmEXP4g$sbc,NA,NA)
BIC_g[5,] = c(mmIG5g$sbc,mmIGAMMA5g$sbc,mmRG5g$sbc,mmEXP5g$sbc,NA,NA)
BIC_g[6,] = c(mmIG6g$sbc,mmIGAMMA6g$sbc,mmRG6g$sbc,mmEXP6g$sbc,NA,NA)
BIC_g[7,] = c(mmIG7g$sbc,mmIGAMMA7g$sbc,mmRG7g$sbc,mmEXP7g$sbc,NA,NA)
BIC_g = cbind(c(1,2,3,4,5,6,7),BIC_g)

colnames(BIC_g) = c('num_of_component','Inverse_Gaussian', 
                    'Inverse_Gamma', 'Reverse_Gumbel', 
                    'Exponential', 'Log_Normal', 'Gaussian')

cols    <- c( "Inverse_Gaussian" = "forestgreen", 
              "Inverse_Gamma" = "lightpink3",
              "Reverse_Gumbel" = "indianred3", 
              "Exponential" = "goldenrod1",
              "Log_Normal" = "deepskyblue3", 
              "Gaussian" = "lemonchiffon4")

shapes     <- c( "Inverse_Gaussian" = 15, "Inverse_Gamma" = 17,
                 "Reverse_Gumbel" = 11, "Exponential" = 18,
                 "Log_Normal" = 25, "Gaussian" = 16)

#BIC plot for paper
ggplot(data=BIC_g) +
  geom_point(aes(x=as.factor(num_of_component), y=Inverse_Gaussian,
                 color = 'Inverse_Gaussian',shape = 'Inverse_Gaussian'),size=3) + 
  geom_point(aes(x=as.factor(num_of_component), y=Inverse_Gamma,
                 color = 'Inverse_Gamma',shape = 'Inverse_Gamma'), size=3) + 
  geom_point(aes(x=as.factor(num_of_component), y=Reverse_Gumbel,
                 color = 'Reverse_Gumbel',shape = 'Reverse_Gumbel'),size=3) + 
  geom_point(aes(x=as.factor(num_of_component), y=Exponential,
                 color = 'Exponential',shape = 'Exponential'),size=3.5) + 
  geom_point(aes(x=as.factor(num_of_component), y=Log_Normal,
                 color = 'Log_Normal',shape = 'Log_Normal'),size=2.7,
             fill = 'deepskyblue3') + 
  geom_point(aes(x=as.factor(num_of_component), y=Gaussian,
                 color = 'Gaussian',shape = 'Gaussian'),size=3) + 
  geom_line(aes(x=as.factor(num_of_component),y=Inverse_Gaussian,group = 1,
                color = 'Inverse_Gaussian'),size = 1) +
  geom_line(aes(x=as.factor(num_of_component), y=Inverse_Gamma,group = 1),
            color = 'lightpink3',size = 1) +
  geom_line(aes(x=as.factor(num_of_component), y=Reverse_Gumbel,group = 1),
            color = 'indianred3',size = 1) +
  geom_line(aes(x=as.factor(num_of_component), y=Exponential,group = 1),
            color = 'goldenrod1',size = 1) +
  geom_line(aes(x=as.factor(num_of_component), y=Log_Normal,group = 1),
            color = 'deepskyblue3',size = 1) +
  geom_line(aes(x=as.factor(num_of_component), y=Gaussian,group = 1),
            color = 'lemonchiffon4',size = 1) +
  scale_color_manual(name = "Model", 
                     breaks = c('Inverse_Gaussian','Inverse_Gamma',
                                'Reverse_Gumbel','Exponential',
                                'Log_Normal','Gaussian'), 
                     values = cols,
                     labels = c('Inverse_Gaussian','Inverse_Gamma',
                                'Reverse_Gumbel','Exponential',
                                'Log_Normal','Gaussian'))+
  labs(x = "Number of Compoents",y = 'BIC',colour = "Model") + 
  scale_shape_manual(name = "Model",
                     breaks = c('Inverse_Gaussian','Inverse_Gamma',
                                'Reverse_Gumbel','Exponential',
                                'Log_Normal','Gaussian'), 
                     values = shapes,
                     labels = c('Inverse_Gaussian','Inverse_Gamma',
                                'Reverse_Gumbel','Exponential',
                                'Log_Normal','Gaussian'))+
  theme(legend.position = c(0.8, 0.7),text = element_text(size=15),
        legend.text = element_text(size=13.5)) +
  geom_text(data=subset(BIC_g, num_of_component == 5),
            aes(x=as.factor(num_of_component), y=Reverse_Gumbel,
                label=round(BIC_g[5,4],2)),
            hjust=0.4, vjust= -0.6,color = 'gray48',size =4 ) +
  geom_text(data=subset(BIC_g, num_of_component == 6),
            aes(x=as.factor(num_of_component), y=Reverse_Gumbel,
                label=round(BIC_g[6,4],2)),
            hjust=0.4, vjust= -0.6,color = 'gray48',size =4 )+
  geom_text(data=subset(BIC_g, num_of_component == 7),
            aes(x=as.factor(num_of_component), y=Reverse_Gumbel,
                label=round(BIC_g[7,4],2)),
            hjust=0.4, vjust= -0.8,color = 'gray48',size =4 )+
  geom_text(data=subset(BIC_g, num_of_component == 3),
            aes(x=as.factor(num_of_component), y=Log_Normal,
                label=round(BIC_g[3,6],2)),
            hjust=0, vjust= 1.2,color = 'gray48',size =4 )


###best fit: 6 component reverse gumbel
fitg1 = data.frame(duration = fit_datag$inXg, 
                   prob = 0.09413079 * dRG(
                     fit_datag$inXg, mu=1140, sigma=exp(4.649)))
fitg2 = data.frame(duration = fit_datag$inXg, 
                   prob = 0.1619269 * dRG(
                     fit_datag$inXg, mu=580.4, sigma=exp(5.479)))
fitg3 = data.frame(duration = fit_datag$inXg, 
                   prob = 0.1477176 * dRG(
                     fit_datag$inXg, mu=120.4, sigma=exp(4.151)))
fitg4 = data.frame(duration = fit_datag$inXg, 
                   prob = 0.08556993 * dRG(
                     fit_datag$inXg, mu=32.14, sigma=exp(2.763)))
fitg5 = data.frame(duration = fit_datag$inXg, 
                   prob = 0.1182684 * dRG(
                     fit_datag$inXg, mu=9.778, sigma=exp(1.267)))
fitg6 = data.frame(duration = fit_datag$inXg, 
                   prob = 0.3923865 * dRG(
                     fit_datag$inXg, mu=2.862, sigma=exp(0.1524)))
fitg = data.frame(duration = fit_datag$inXg, 
                  prob = dMX(fit_datag$inXg, 
                             mu=list(1140,580.4,120.4,32.14,9.778,2.862), 
                             sigma=list(exp(4.649),exp(5.479),exp(4.151),
                                        exp(2.763),exp(1.267),exp(0.1524)), 
                             pi = list(0.09413079, 0.1619269, 0.1477176, 
                                       0.08556993, 0.1182684, 0.3923865),
                             family=list("RG","RG","RG","RG","RG","RG")))

#classification result for paper
ggplot(miss_list, aes(x=duration)) + 
  geom_histogram(aes(y=..density..), position="identity",binwidth = 1) + 
  xlim(c(0, 60)) + labs(x = "gap duration (1-60min)",y = 'prob') + 
  theme(text = element_text(size=15)) +
  geom_line(data = fitg1,aes(x=duration, y=prob),
            color = 'forestgreen',size = 0.5) +
  geom_line(data = fitg2,aes(x=duration, y=prob),
            color = 'lightpink3',size = 0.5) +
  geom_line(data = fitg3,aes(x=duration, y=prob),
            color = 'goldenrod1',size = 0.5) +
  geom_line(data = fitg4,aes(x=duration, y=prob),
            color = 'lemonchiffon4',size = 0.5) +
  geom_line(data = fitg5,aes(x=duration, y=prob),
            color = 'deepskyblue3',size = 0.5) +
  geom_line(data = fitg6,aes(x=duration, y=prob),
            color = 'darkslateblue',size = 0.5) +
  geom_line(data = fitg,aes(x=duration, y=prob),
            color = 'red',size = 1) 

#get temporal duration cut off value
##cut off value1
for (i in 1:1439) {
  if(0.3923865 * dRG(i, mu=2.862, sigma=exp(0.1524)) < 0.1182684 * 
     dRG(i, mu=9.778, sigma=exp(1.267))){
    print(i)
    break
  }
}
##cut off value2
##cut off value3
##cut off value4
##cut off value5

#Step2: Spatio-temporal: distance-speed clustering----
#pick the best clustering method based on gap_stat value for classification

##for small duration group-----
#normalization
miss_small = miss_list[which(miss_list$duration<8),]
miss_small$distance_sd = (miss_small$distance-min(miss_small$distance))/
  (max(miss_small$distance)-min(miss_small$distance))
miss_small$speed_sd = (miss_small$speed-min(miss_small$speed))/
  (max(miss_small$speed)-min(miss_small$speed))
x = as.matrix(miss_small[,c('distance_sd','speed_sd')])

####method family 1: hierarchical clustering----
d <- dist(as.matrix(x))
hclust4 <- hclust(d, method = "ward.D")

wss = get_nbclust(x, FUN = hcut, method = "wss",print.summary=TRUE,k.max = 8)
gap_stat <- get_nbclust(x, FUN = hcut, method = "gap_stat",
                        print.summary=TRUE,k.max = 8)

hcluster_g1 = data.frame(cbind(num=1:8,wss = wss$y, gap = gap_stat$Tab))
colnames(hcluster_g1)[6] = 'SEsim'

#gap_stat plot to decide optimal number of clusters
ggplot(data = hcluster_g1,aes(x = num))+
  geom_line(aes(y=wss,color = 'WSS'),) +
  geom_point(aes(y=wss),color = 'steelblue') +
  geom_line(aes(y=gap*5,color = 'Gap Stat'),) +
  geom_point(aes(y=gap*5),color = 'indianred3') +
  geom_errorbar(aes(y=gap*5,ymin=(gap-SEsim)*5, ymax=(gap+SEsim)*5), 
                colour="indianred3", width=.1) +
  scale_y_continuous(name = 'Total Within Sum of Square',
                     sec.axis = sec_axis(~.*1/5, name = "Gap Statistics")) +
  scale_colour_manual(values = c("indianred3","steelblue")) +
  labs(y = "Total Within Sum of Square",x = "Number of clusters",
       colour = "Creterion")+
  theme(legend.position = c(0.87, 0.5),text = element_text(size=15),
        legend.text = element_text(size=14))+
  geom_vline(xintercept = 5, linetype = 2,color = 'steelblue')

sub_grp <- cutree(hclust4, k = 6)
miss_small_hcut = cbind(miss_small,sub_grp) #get cluster data.frame
colnames(miss_small_hcut)[length(miss_small_hcut)] = 'group'
miss_small_hcut$group=as.character(miss_small_hcut$group)

cols    <- c( "1" = "indianred3", "2" = "springgreen4",
              "3" = "goldenrod3", "4" = "deepskyblue3","5" = "deeppink3")

#clustering results
ggplot(data=miss_small_hcut, aes(x=distance, y=speed,color=group)) + 
  geom_point()+
  labs(x = "distance (km)",y = 'speed (m/s)') + 
  theme(text = element_text(size=15),legend.text = element_text(size=14))+
  scale_color_manual(name = "group", breaks = c('1','2','3','4','5'), 
                     values = cols,labels = c('1','2','3','4','5'))

#clustering results(zoomed-in)
ggplot(data=miss_small_hcut[which(miss_small_hcut$speed<5),], 
       aes(x=distance, y=speed,color=group)) + geom_point() +
  labs(x = "distance (km)",y = 'speed (m/s)') + 
  theme(text = element_text(size=15),legend.text = element_text(size=14))+
  scale_color_manual(name = "group", breaks = c('1','2','3','4','5'), 
                     values = cols,labels = c('1','2','3','4','5'))

####method family 2: k-means clustering (best) ------
wss = get_nbclust(x, FUN = kmeans, method = "wss",print.summary=TRUE,k.max = 8)
gap_stat <- get_nbclust(x, FUN = kmeans, method = "gap_stat",
                        print.summary=TRUE,k.max = 8)

kcluster_g1 = data.frame(cbind(num=1:8,wss = wss$y, gap = gap_stat$Tab))
colnames(kcluster_g1)[6] = 'SEsim'

#gap_stat plot to decide optimal numer of clusters
ggplot(data = kcluster_g1,aes(x = num))+
  geom_line(aes(y=wss,color = 'WSS'),) +
  geom_point(aes(y=wss),color = 'steelblue') +
  geom_line(aes(y=gap*5,color = 'Gap Stat'),) +
  geom_point(aes(y=gap*5),color = 'indianred3') +
  geom_errorbar(aes(y=gap*5,ymin=(gap-SEsim)*5, ymax=(gap+SEsim)*5), 
                colour="indianred3", width=.1) +
  scale_y_continuous(name = 'Total Within Sum of Square',
                     sec.axis = sec_axis(~.*1/5, name = "Gap Statistics")) +
  scale_colour_manual(values = c("indianred3","steelblue")) +
  labs(y = "Total Within Sum of Square",x = "Number of clusters",
       colour = "Creterion")+
  theme(legend.position = c(0.7, 0.5),text = element_text(size=15),
        legend.text = element_text(size=14))+
  geom_vline(xintercept = 7, linetype = 2,color = 'steelblue')


set.seed(123)
a.res <- kmeans(x,7)
miss_small_cluster = cbind(miss_small,a.res$cluster) #get cluster data.frame
colnames(miss_small_cluster)[length(miss_small_cluster)] = 'group'
miss_small_cluster$group=as.character(miss_small_cluster$group)

cols    <- c( "1" = "indianred3", "2" = "springgreen4",
              "3" = "goldenrod3", "4" = "deepskyblue3",
              "5" = "deeppink3", "6" = "cadetblue4","7" = "blueviolet" )

#clustering results
ggplot(data=miss_small_cluster, aes(x=distance, y=speed,color=group)) + 
  geom_point()+
  labs(x = "distance (km)",y = 'speed (m/s)')+ 
  theme(text = element_text(size=15),legend.text = element_text(size=14))+
  scale_color_manual(name = "group", breaks = c('1','2','3','4','5','6','7'), 
                     values = cols,labels = c('1','2','3','4','5','6','7'))
#clustering results(zoomed-in)
ggplot(data=miss_small_cluster[which(miss_small_cluster$speed<5),], 
       aes(x=distance, y=speed,color=group)) + geom_point() +
  labs(x = "distance (km)",y = 'speed (m/s)')+ 
  theme(text = element_text(size=15),legend.text = element_text(size=14))+
  scale_color_manual(name = "group", breaks = c('1','2','3','4','5','6','7'), 
                     values = cols,labels = c('1','2','3','4','5','6','7'))

####method family 3: fuzzy cluster ------
wss = get_nbclust(x, FUN = cluster::fanny, method = "wss",
                  print.summary=TRUE,k.max = 8)
gap_stat <- get_nbclust(x, FUN = cluster::fanny, method = "gap_stat",
                        print.summary=TRUE,k.max = 8)

fcluster_g1 = data.frame(cbind(num=1:8,wss = wss$y, gap = gap_stat$Tab))
colnames(fcluster_g1)[6] = 'SEsim'

#gap_stat plot to decide optimal number of clusters
ggplot(data = fcluster_g1,aes(x = num))+
  geom_line(aes(y=wss,color = 'WSS'),) +
  geom_point(aes(y=wss),color = 'steelblue') +
  geom_line(aes(y=gap*5,color = 'Gap Stat'),) +
  geom_point(aes(y=gap*5),color = 'indianred3') +
  geom_errorbar(aes(y=gap*5,ymin=(gap-SEsim)*5, ymax=(gap+SEsim)*5), 
                colour="indianred3", width=.1) +
  scale_y_continuous(name = 'Total Within Sum of Square', 
                     sec.axis = sec_axis(~.*1/5, name = "Gap Statistics")) +
  scale_colour_manual(values = c("indianred3","steelblue")) +
  labs(y = "Total Within Sum of Square",x = "Number of clusters",
       colour = "Creterion")+
  theme(legend.position = c(0.7, 0.5),text = element_text(size=15),
        legend.text = element_text(size=14))+
  geom_vline(xintercept = 7, linetype = 2,color = 'steelblue')

fannyx <- fanny(x, 7,stand=FALSE)
miss_small_fanny = cbind(miss_small,fannyx$clustering) #get cluster data.frame
colnames(miss_small_fanny)[length(miss_small_fanny)] = 'group'
miss_small_fanny$group=as.character(miss_small_fanny$group)

cols    <- c( "1" = "indianred3", "2" = "springgreen4",
              "3" = "goldenrod3", "4" = "deepskyblue3",
              "5" = "deeppink3", "6" = "cadetblue4","7" = "blueviolet" )

#clustering results
ggplot(data=miss_small_fanny, aes(x=distance, y=speed,color=group)) + 
  geom_point() +
  labs(x = "distance (km)",y = 'speed (m/s)')+ 
  theme(text = element_text(size=15),legend.text = element_text(size=14))+
  scale_color_manual(name = "group", breaks = c('1','2','3','4','5','6','7'), 
                     values = cols,labels = c('1','2','3','4','5','6','7'))

#clustering results(zoomed-in)
ggplot(data=miss_small_fanny[which(miss_small_fanny$speed<5),], 
       aes(x=distance, y=speed,color=group)) + geom_point() +
  labs(x = "distance (km)",y = 'speed (m/s)')+ 
  theme(text = element_text(size=15),legend.text = element_text(size=14))+
  scale_color_manual(name = "group", breaks = c('1','2','3','4','5','6','7'), 
                     values = cols,labels = c('1','2','3','4','5','6','7'))

##for medium duration group-----
#normalization
miss_medium = miss_list[which(miss_list$duration>=8 & miss_list$duration<21),]
miss_medium = miss_medium[which(miss_medium$speed<90),]
miss_medium$distance_sd = (miss_medium$distance-min(miss_medium$distance))/
  (max(miss_medium$distance)-min(miss_medium$distance))
miss_medium$speed_sd = (miss_medium$speed-min(miss_medium$speed))/
  (max(miss_medium$speed)-min(miss_medium$speed))
x = as.matrix(miss_medium[,c('distance_sd','speed_sd')])

####method family 1: hierarchical clustering----
d <- dist(as.matrix(x))
hclust4 <- hclust(d, method = "ward.D")

wss = get_nbclust(x, FUN = hcut, method = "wss",print.summary=TRUE,k.max = 8)
gap_stat <- get_nbclust(x, FUN = hcut, method = "gap_stat",
                        print.summary=TRUE,k.max = 8)

hcluster_g2 = data.frame(cbind(num=1:8,wss = wss$y, gap = gap_stat$Tab))
colnames(hcluster_g2)[6] = 'SEsim'

#gap_stat plot to decide optimal number of clusters
ggplot(data = hcluster_g2,aes(x = num))+
  geom_line(aes(y=wss,color = 'WSS'),) +
  geom_point(aes(y=wss),color = 'steelblue') +
  geom_line(aes(y=gap*5,color = 'Gap Stat'),) +
  geom_point(aes(y=gap*5),color = 'indianred3') +
  geom_errorbar(aes(y=gap*5,ymin=(gap-SEsim)*5, ymax=(gap+SEsim)*5), 
                colour="indianred3", width=.1) +
  scale_y_continuous(name = 'Total Within Sum of Square',
                     sec.axis = sec_axis(~.*1/5, name = "Gap Statistics")) +
  scale_colour_manual(values = c("indianred3","steelblue")) +
  labs(y = "Total Within Sum of Square",x = "Number of clusters",
       colour = "Creterion")+
  theme(legend.position = c(0.68, 0.85),text = element_text(size=15),
        legend.text = element_text(size=14))+
  geom_vline(xintercept = 7, linetype = 2,color = 'steelblue')


sub_grp <- cutree(hclust4, k = 7)
miss_medium_hcut = cbind(miss_medium,sub_grp) #get cluster data.frame
colnames(miss_medium_hcut)[length(miss_medium_hcut)] = 'group'
miss_medium_hcut$group=as.character(miss_medium_hcut$group)

cols    <- c( "1" = "indianred3", "2" = "springgreen4",
              "3" = "goldenrod3", "4" = "deepskyblue3",
              "5" = "deeppink3", "6" = "cadetblue4","7" = "blueviolet" )

#clustering results
ggplot(data=miss_medium_hcut, aes(x=distance, y=speed,color=group)) + 
  geom_point()+
  labs(x = "distance (km)",y = 'speed (m/s)')+ 
  theme(text = element_text(size=15),legend.text = element_text(size=14))+
  scale_color_manual(name = "group", breaks = c('1','2','3','4','5','6','7'), 
                     values = cols,labels = c('1','2','3','4','5','6','7'))

#clustering results(zoomed-in)
ggplot(data=miss_medium_hcut[which(miss_medium_hcut$speed<5),], 
       aes(x=distance, y=speed,color=group)) + geom_point() +
  labs(x = "distance (km)",y = 'speed (m/s)') + 
  theme(text = element_text(size=15),legend.text = element_text(size=14))+
  scale_color_manual(name = "group", breaks = c('1','2','3','4','5','6','7'), 
                     values = cols,labels = c('1','2','3','4','5','6','7'))

####method family 2: k-means clustering----
wss = get_nbclust(x, FUN = kmeans, method = "wss",print.summary=TRUE,k.max = 8)
gap_stat <- get_nbclust(x, FUN = kmeans, method = "gap_stat",
                        print.summary=TRUE,k.max = 8)

kcluster_g2 = data.frame(cbind(num=1:8,wss = wss$y, gap = gap_stat$Tab))
colnames(kcluster_g2)[6] = 'SEsim'

#gap_stat plot to decide optimal number of clusters
ggplot(data = kcluster_g2,aes(x = num))+
  geom_line(aes(y=wss,color = 'WSS'),) +
  geom_point(aes(y=wss),color = 'steelblue') +
  geom_line(aes(y=gap*5,color = 'Gap Stat'),) +
  geom_point(aes(y=gap*5),color = 'indianred3') +
  geom_errorbar(aes(y=gap*5,ymin=(gap-SEsim)*5, ymax=(gap+SEsim)*5), colour="indianred3", width=.1) +
  scale_y_continuous(name = 'Total Within Sum of Square',
                     sec.axis = sec_axis(~.*1/5, name = "Gap Statistics")) +
  scale_colour_manual(values = c("indianred3","steelblue")) +
  labs(y = "Total Within Sum of Square",x = "Number of clusters",colour = "Creterion")+
  theme(legend.position = c(0.87, 0.5))+
  geom_vline(xintercept = 7, linetype = 2,color = 'steelblue')

set.seed(123)
a.res <- kmeans(x,7)
miss_medium_cluster = cbind(miss_medium,a.res$cluster) 
colnames(miss_medium_cluster)[length(miss_medium_cluster)] = 'group'
miss_medium_cluster$group=as.character(miss_medium_cluster$group)

#clustering results
ggplot(data=miss_medium_cluster, aes(x=distance, y=speed,color=group)) + 
  geom_point()+
  labs(x = "distance (km)",y = 'speed (m/s)')

#clustering results(zoomed-in)
ggplot(data=miss_medium_cluster[which(miss_medium_cluster$speed<5),], 
       aes(x=distance, y=speed,color=group)) + geom_point() +
  labs(x = "distance (km)",y = 'speed (m/s)')

####method family 3: fuzzy cluster ------
wss = get_nbclust(x, FUN = cluster::fanny, method = "wss",
                  print.summary=TRUE,k.max = 8)
gap_stat <- get_nbclust(x, FUN = cluster::fanny, method = "gap_stat",
                        print.summary=TRUE,k.max = 8)

fcluster_g2 = data.frame(cbind(num=1:8,wss = wss$y, gap = gap_stat$Tab))
colnames(fcluster_g2)[6] = 'SEsim'

#gap_stat plot to decide optimal number of clusters
ggplot(data = fcluster_g2,aes(x = num))+
  geom_line(aes(y=wss,color = 'WSS'),) +
  geom_point(aes(y=wss),color = 'steelblue') +
  geom_line(aes(y=gap*4,color = 'Gap Stat'),) +
  geom_point(aes(y=gap*4),color = 'indianred3') +
  geom_errorbar(aes(y=gap*4,ymin=(gap-SEsim)*4, ymax=(gap+SEsim)*4), colour="indianred3", width=.1) +
  scale_y_continuous(name = 'Total Within Sum of Square', 
                     sec.axis = sec_axis(~.*1/4, name = "Gap Statistics")) +
  scale_colour_manual(values = c("indianred3","steelblue")) +
  labs(y = "Total Within Sum of Square",x = "Number of clusters",colour = "Creterion")+
  theme(legend.position = c(0.87, 0.85)) +
  geom_vline(xintercept = 6, linetype = 2,color = 'steelblue')

fannyx <- fanny(x,3,stand=FALSE)
miss_medium_fanny = cbind(miss_medium,fannyx$clustering) 
colnames(miss_medium_fanny)[length(miss_medium_fanny)] = 'group'
miss_medium_fanny$group=as.character(miss_medium_fanny$group)

cols    <- c( "1" = "indianred3", "2" = "springgreen4",
              "3" = "goldenrod3", "4" = "deepskyblue3",
              "5" = "deeppink3", "6" = "cadetblue4")

#clustering results
ggplot(data=miss_medium_fanny, aes(x=distance, y=speed,color=group)) + geom_point() +
  labs(x = "distance (km)",y = 'speed (m/s)')+ 
  scale_color_manual(name = "group", breaks = c('1','2','3','4','5','6'), 
                     values = cols,labels = c('1','2','3','4','5','6'))

#clustering results(zoomed-in)
ggplot(data=miss_medium_fanny[which(miss_medium_fanny$speed<5),], 
       aes(x=distance, y=speed,color=group)) + geom_point() +
  labs(x = "distance (km)",y = 'speed (m/s)')+ 
  scale_color_manual(name = "group", breaks = c('1','2','3','4','5','6'), 
                     values = cols,labels = c('1','2','3','4','5','6'))

##for large duration group-----
#normalization
miss_large = miss_list[which(miss_list$duration>=21 & miss_list$duration<68),]
miss_large$distance_sd = (miss_large$distance-min(miss_large$distance))/
  (max(miss_large$distance)-min(miss_large$distance))
miss_large$speed_sd = (miss_large$speed-min(miss_large$speed))/
  (max(miss_large$speed)-min(miss_large$speed))
x = as.matrix(miss_large[,c('distance_sd','speed_sd')])

####method family 1: hierarchical clustering----
d <- dist(as.matrix(x))
hclust4 <- hclust(d, method = "ward.D")

wss = get_nbclust(x, FUN = hcut, method = "wss",print.summary=TRUE,k.max = 8)
gap_stat <- get_nbclust(x, FUN = hcut, method = "gap_stat",
                        print.summary=TRUE,k.max = 8)

hcluster_g3 = data.frame(cbind(num=1:8,wss = wss$y, gap = gap_stat$Tab))
colnames(hcluster_g3)[6] = 'SEsim'

#gap_stat plot to decide optimal number of clusters
ggplot(data = hcluster_g3,aes(x = num))+
  geom_line(aes(y=wss,color = 'WSS'),) +
  geom_point(aes(y=wss),color = 'steelblue') +
  geom_line(aes(y=gap*5,color = 'Gap Stat'),) +
  geom_point(aes(y=gap*5),color = 'indianred3') +
  geom_errorbar(aes(y=gap*5,ymin=(gap-SEsim)*5, ymax=(gap+SEsim)*5), 
                colour="indianred3", width=.1) +
  scale_y_continuous(name = 'Total Within Sum of Square',
                     sec.axis = sec_axis(~.*1/5, name = "Gap Statistics")) +
  scale_colour_manual(values = c("indianred3","steelblue")) +
  labs(y = "Total Within Sum of Square",x = "Number of clusters",
       colour = "Creterion")+
  theme(legend.position = c(0.82, 0.6),text = element_text(size=15),
        legend.text = element_text(size=14))+
  geom_vline(xintercept = 8, linetype = 2,color = 'steelblue')

sub_grp <- cutree(hclust4, k = 8)
miss_large_hcut = cbind(miss_large,sub_grp) #get cluster data.frame
colnames(miss_large_hcut)[length(miss_large_hcut)] = 'group'
miss_large_hcut$group=as.character(miss_large_hcut$group)

cols    <- c( "1" = "indianred3", "2" = "springgreen4",
              "3" = "goldenrod3", "4" = "deepskyblue3",
              "5" = "deeppink3", "6" = "cadetblue4",
              "7" = "blueviolet" , "8" = "salmon4")

#clustering results
ggplot(data=miss_large_hcut, aes(x=distance, y=speed,color=group)) + 
  geom_point()+
  labs(x = "distance (km)",y = 'speed (m/s)') + 
  theme(text = element_text(size=15),legend.text = element_text(size=14))+
  scale_color_manual(name = "group", breaks = c('1','2','3','4','5','6','7','8'), 
                     values = cols,labels = c('1','2','3','4','5','6','7','8'))

#clustering results(zoomed-in)
ggplot(data=miss_large_hcut[which(miss_large_hcut$speed<5),], 
       aes(x=distance, y=speed,color=group)) + geom_point() +
  labs(x = "distance (km)",y = 'speed (m/s)') + 
  theme(text = element_text(size=15),legend.text = element_text(size=14))+
  scale_color_manual(name = "group", breaks = c('1','2','3','4','5','6','7','8'), 
                     values = cols,labels = c('1','2','3','4','5','6','7','8'))

####method family 2: k-means clustering----
wss = get_nbclust(x, FUN = kmeans, method = "wss",print.summary=TRUE,k.max = 8)
gap_stat <- get_nbclust(x, FUN = kmeans, method = "gap_stat",
                        print.summary=TRUE,k.max = 8)

kcluster_g3 = data.frame(cbind(num=1:8,wss = wss$y, gap = gap_stat$Tab))
colnames(kcluster_g3)[6] = 'SEsim'

#gap_stat plot to decide optimal number of clusters
ggplot(data = kcluster_g3,aes(x = num))+
  geom_line(aes(y=wss,color = 'WSS'),) +
  geom_point(aes(y=wss),color = 'steelblue') +
  geom_line(aes(y=gap*2.5,color = 'Gap Stat'),) +
  geom_point(aes(y=gap*2.5),color = 'indianred3') +
  geom_errorbar(aes(y=gap*2.5,ymin=(gap-SEsim)*2.5, ymax=(gap+SEsim)*2.5), 
                colour="indianred3", width=.1) +
  scale_y_continuous(name = 'Total Within Sum of Square',
                     sec.axis = sec_axis(~.*1/2.5, name = "Gap Statistics")) +
  scale_colour_manual(values = c("indianred3","steelblue")) +
  labs(y = "Total Within Sum of Square",x = "Number of clusters",
       colour = "Creterion")+
  theme(legend.position = c(0.87, 0.5))+
  geom_vline(xintercept = 4, linetype = 2,color = 'steelblue')


set.seed(123)
a.res <- kmeans(x,4)
miss_large_cluster = cbind(miss_large,a.res$cluster) #get cluster data.frame
colnames(miss_large_cluster)[length(miss_large_cluster)] = 'group'
miss_large_cluster$group=as.character(miss_large_cluster$group)

cols    <- c( "1" = "indianred3", "2" = "springgreen4",
              "3" = "goldenrod3", "4" = "deepskyblue3",
              "5" = "deeppink3", "6" = "cadetblue4")

#clustering results
ggplot(data=miss_large_cluster, aes(x=distance, y=speed,color=group)) + geom_point()+
  labs(x = "Distance (km)",y = 'Speed (m/s)')+ 
  scale_color_manual(name = "group", breaks = c('1','2','3','4','5','6'), 
                     values = cols,labels = c('1','2','3','4','5','6')) +
  theme(text = element_text(size=15),legend.text = element_text(size=14)) 

#clustering results(zoomed-in)
ggplot(data=miss_large_cluster[which(miss_large_cluster$speed<5),], 
       aes(x=distance, y=speed,color=group)) + geom_point() +
  labs(x = "distance (km)",y = 'speed (m/s)')+ 
  scale_color_manual(name = "group", breaks = c('1','2','3','4','5','6'), 
                     values = cols,labels = c('1','2','3','4','5','6'))

####method family 3: fuzzy cluster ------
wss = get_nbclust(x, FUN = cluster::fanny, method = "wss",
                  print.summary=TRUE,k.max = 8)
gap_stat <- get_nbclust(x, FUN = cluster::fanny, method = "gap_stat",
                        print.summary=TRUE,k.max = 8)

fcluster_g3 = data.frame(cbind(num=1:8,wss = wss$y, gap = gap_stat$Tab))
colnames(fcluster_g3)[6] = 'SEsim'

#gap_stat plot to decide optimal number of clusters
ggplot(data = fcluster_g3,aes(x = num))+
  geom_line(aes(y=wss,color = 'WSS'),) +
  geom_point(aes(y=wss),color = 'steelblue') +
  geom_line(aes(y=gap*5,color = 'Gap Stat'),) +
  geom_point(aes(y=gap*5),color = 'indianred3') +
  geom_errorbar(aes(y=gap*5,ymin=(gap-SEsim)*5, ymax=(gap+SEsim)*5), 
                colour="indianred3", width=.1) +
  scale_y_continuous(name = 'Total Within Sum of Square', 
                     sec.axis = sec_axis(~.*1/5, name = "Gap Statistics")) +
  scale_colour_manual(values = c("indianred3","steelblue")) +
  labs(y = "Total Within Sum of Square",x = "Number of clusters",
       colour = "Creterion")+
  theme(legend.position = c(0.87, 0.75)) +
  geom_vline(xintercept = 3, linetype = 2,color = 'steelblue')

gap <- gap_stat$Tab[1:8, "gap"]
se <- gap_stat$Tab[1:8, "SE.sim"]
maxSE(gap, se, method = 'firstmax', SE.factor = maxSE$SE.factor)

fannyx <- fanny(x,3,stand=FALSE)
miss_large_fanny = cbind(miss_large,fannyx$clustering) #get cluster data.frame
colnames(miss_large_fanny)[length(miss_large_fanny)] = 'group'
miss_large_fanny$group=as.character(miss_large_fanny$group)

#clustering results
ggplot(data=miss_large_fanny, aes(x=distance, y=speed,color=group)) + 
  geom_point() +
  labs(x = "distance (km)",y = 'speed (m/s)')

#clustering results(zoomed-in)
ggplot(data=miss_large_fanny[which(miss_large_fanny$speed<5),], 
       aes(x=distance, y=speed,color=group)) + geom_point() +
  labs(x = "distance (km)",y = 'speed (m/s)')

#Step3: Label ST Group for further interpolation----
#define data loss cases
actr[which(actr$subtype=='MI'&actr$duration>=68),'label'] = 'Data Loss'
for (i in 1:length(actr$TripId)) {
  if(actr$subtype[i]=='MI' & actr$label[i]=='Data Loss'& 
     actr$special[i]==1 ){
    if(actr$startlat[i-1]==-999 & actr$UserId[i-1]==actr$UserId[i] & 
       actr$special[i-1]==1){
      actr$label[i-1]='Data Loss'
    }
    else if(actr$startlat[i+1]==-999 & actr$UserId[i+1]==actr$UserId[i] & 
            actr$special[i+1]==1){
      actr$label[i+1]='Data Loss'
    }
  }
}
actr[which(actr$special==2),'label'] = 'Data Loss'

#label ST-activity-travel group
for (i in 1:length(actr$TripId)) {
  if(actr$subtype[i]=='MI'){
    for(j in 1:length(miss_small_cluster$UserId)){
      if(actr$TripId[i-1]==miss_small_cluster$TripId[j]){
        if(miss_small_cluster$group[j]==1){
          actr$label[i] = '1.1'
        }
        else if(miss_small_cluster$group[j]==2){
          actr$label[i] = '1.2'
        }
        else if(miss_small_cluster$group[j]==3){
          actr$label[i] = '1.3'
        }
        else if(miss_small_cluster$group[j]!=1&miss_small_cluster$group[j]!=2&
                miss_small_cluster$group[j]!=3){
          actr$label[i] = '1.4'
        }
        if(miss_small_cluster$case[j]==1){
          actr$label[i] = paste(actr$label[i],'AA',sep = '_')
        }
        else if(miss_small_cluster$case[j]==2){
          actr$label[i] = paste(actr$label[i],'TA',sep = '_')
        }
        else if(miss_small_cluster$case[j]==3){
          actr$label[i] = paste(actr$label[i],'AT',sep = '_')
        }
        else if(miss_small_cluster$case[j]==4){
          actr$label[i] = paste(actr$label[i],'TT',sep = '_')
        }
      }
    }
  }
}    

for (i in 1:length(actr$TripId)) {
  if(actr$subtype[i]=='MI'){
    for(j in 1:length(miss_medium_hcut$UserId)){
      if(actr$TripId[i-1]==miss_medium_hcut$TripId[j]){
        if(miss_medium_hcut$group[j]==1){
          actr$label[i] = '2.1'
        }
        else if(miss_medium_hcut$group[j]==2){
          actr$label[i] = '2.2'
        }
        else if(miss_medium_hcut$group[j]==3){
          actr$label[i] = '2.3'
        }
        else if(miss_medium_hcut$group[j]!=1&miss_medium_hcut$group[j]!=2&
                miss_medium_hcut$group[j]!=3){
          actr$label[i] = '2.4'
        }
        if(miss_medium_hcut$case[j]==1){
          actr$label[i] = paste(actr$label[i],'AA',sep = '_')
        }
        else if(miss_medium_hcut$case[j]==2){
          actr$label[i] = paste(actr$label[i],'TA',sep = '_')
        }
        else if(miss_medium_hcut$case[j]==3){
          actr$label[i] = paste(actr$label[i],'AT',sep = '_')
        }
        else if(miss_medium_hcut$case[j]==4){
          actr$label[i] = paste(actr$label[i],'TT',sep = '_')
        }
      }
    }
  }
}

for (i in 1:length(actr$TripId)) {
  if(actr$subtype[i]=='MI'){
    for(j in 1:length(miss_large_hcut$UserId)){
      if(actr$TripId[i-1]==miss_large_hcut$TripId[j]){
        if(miss_large_hcut$group[j]==1 |miss_large_hcut$group[j]==2){
          actr$label[i] = '3.1'
        }
        else if(miss_large_hcut$group[j]==3){
          actr$label[i] = '3.2'
        }
        else if(miss_large_hcut$group[j]==4|miss_large_hcut$group[j]==5){
          actr$label[i] = '3.3'
        }
        else if(miss_large_hcut$group[j]==6|miss_large_hcut$group[j]==7|
                miss_large_hcut$group[j]==8){
          actr$label[i] = '3.4'
        }
        if(miss_large_hcut$case[j]==1){
          actr$label[i] = paste(actr$label[i],'AA',sep = '_')
        }
        else if(miss_large_hcut$case[j]==2){
          actr$label[i] = paste(actr$label[i],'TA',sep = '_')
        }
        else if(miss_large_hcut$case[j]==3){
          actr$label[i] = paste(actr$label[i],'AT',sep = '_')
        }
        else if(miss_large_hcut$case[j]==4){
          actr$label[i] = paste(actr$label[i],'TT',sep = '_')
        }
      }
    }
  }
}

#Step4: Visualize activity-travel information for interpolation----
#get melted_matrix
melted_matrix = function(miss){
  #initialization
  miss_matrix = data.frame(matrix(0, nrow = 16, ncol = 16))
  colnames(miss_matrix) = c('HO','WO','ED','PB','EO','LR','SH','OA',
                            'CA','BU','RA','IV','BI','WL','WI','OT')
  rownames(miss_matrix) = c('HO','WO','ED','PB','EO','LR','SH','OA',
                            'CA','BU','RA','IV','BI','WL','WI','OT')
  
  melted_miss_matrix = melt(as.matrix(miss_matrix))
  melted_miss_matrix = cbind(melted_miss_matrix,
                             data.frame(matrix(NaN, nrow = 256, ncol = 12)))
  colnames(melted_miss_matrix) = c('from','to','count','ave_spd','max_spd',
                                   'min_spd','ave_dist','max_dist','min_dist',
                                   'ave_dur','max_dur','min_dur')
  #calculate
  for (i in 1:length(melted_miss_matrix$from)) {
    attribute = data.frame(cbind(NA,NA,NA))
    colnames(attribute) = c('speed','distance','duration')
    for(j in 1:length(miss$from)){
      if(melted_miss_matrix$from[i]==miss$from[j]&
         melted_miss_matrix$to[i]==miss$to[j]){
        melted_miss_matrix$count[i] = melted_miss_matrix$count[i] + 1
        a = data.frame(cbind(miss$speed[j],miss$distance[j],miss$duration[j]))
        colnames(a) = c('speed','distance','duration')
        attribute=rbind(attribute,a)
      }
    }
    attribute = attribute[-1,]
    melted_miss_matrix$ave_spd[i] = mean(attribute$speed)
    melted_miss_matrix$max_spd[i] = max(attribute$speed)
    melted_miss_matrix$min_spd[i] = min(attribute$speed)
    melted_miss_matrix$ave_dist[i] = mean(attribute$distance)
    melted_miss_matrix$max_dist[i] = max(attribute$distance)
    melted_miss_matrix$min_dist[i] = min(attribute$distance)
    melted_miss_matrix$ave_dur[i] = mean(attribute$duration)
    melted_miss_matrix$max_dur[i] = max(attribute$duration)
    melted_miss_matrix$min_dur[i] = min(attribute$duration)
  }
  return(melted_miss_matrix[order(melted_miss_matrix$count,decreasing = TRUE),])
}

#data preparation
miss = miss_small_cluster[which(miss_small_cluster$group==1),]
miss_matrix_1.1 = melted_matrix(miss)
miss = miss_small_cluster[which(miss_small_cluster$group==2),]
miss_matrix_1.2 = melted_matrix(miss)
miss = miss_small_cluster[which(miss_small_cluster$group==3),]
miss_matrix_1.3 = melted_matrix(miss)
miss = miss_small_cluster[which(miss_small_cluster$group!=1&
                                  miss_small_cluster$group!=2&
                                  miss_small_cluster$group!=3),]
miss_matrix_1.4 = melted_matrix(miss)

miss = miss_medium_hcut[which(miss_medium_hcut$group==1),]
miss_matrix_2.1 = melted_matrix(miss)
miss = miss_medium_hcut[which(miss_medium_hcut$group==2),]
miss_matrix_2.2 = melted_matrix(miss)
miss = miss_medium_hcut[which(miss_medium_hcut$group==3),]
miss_matrix_2.3 = melted_matrix(miss)
miss = miss_medium_hcut[which(miss_medium_hcut$group!=1 & 
                                miss_medium_hcut$group!=2 & 
                                miss_medium_hcut$group!=3),]
miss_matrix_2.4 = melted_matrix(miss)

miss = miss_large_hcut[which(miss_large_hcut$group==1|
                               miss_large_hcut$group==2),]
miss_matrix_3.1 = melted_matrix(miss)
miss = miss_large_hcut[which(miss_large_hcut$group==3),]
miss_matrix_3.2 = melted_matrix(miss)
miss = miss_large_hcut[which(miss_large_hcut$group==4|
                               miss_large_hcut$group==5),]
miss_matrix_3.3 = melted_matrix(miss)
miss = miss_large_hcut[which(miss_large_hcut$group==6|
                               miss_large_hcut$group==7|
                               miss_large_hcut$group==8),]
miss_matrix_3.4 = melted_matrix(miss)

melted_miss_matrix = miss_matrix_3.4

melted_miss_matrix$from <- factor(melted_miss_matrix$from , 
                                  levels=c('HO','WO','ED','PB','EO','LR','SH',
                                           'OA',"CA",  "BU","IV",'RA',"BI",
                                           'WL','WI','OT' ))
melted_miss_matrix$to <- factor(melted_miss_matrix$to , 
                                levels=c('HO','WO','ED','PB','EO','LR','SH',
                                         'OA',"CA",  "BU","IV",'RA',"BI",
                                         'WL','WI','OT' ))

#transition matrix visualization
for (i in 1:length(melted_miss_matrix$from)) {
  if(melted_miss_matrix$count[i]==0){
    melted_miss_matrix$count[i]=NaN
  }
}

table(melted_miss_matrix$count[which(!is.na(melted_miss_matrix$count))])
quantile(melted_miss_matrix$count[which(!is.na(melted_miss_matrix$count))])

pal <- wes_palette("Zissou1", 5, type = "discrete")[c(1,3)]
size_level = cut(melted_miss_matrix$ave_spd,
                 quantile(melted_miss_matrix$ave_spd[
                   which(!is.na(melted_miss_matrix$count))]),
                 #c(0,0.008,0.018,0.056,0.223),
                 include.lowest = TRUE)
size_level=size_level[!is.na(size_level)]

ggplot(data = melted_miss_matrix, 
       aes(x=to, y=from, fill=cut(count,c(0,1,2)))) + 
  xlab("to") + ylab("from") +  geom_tile(colour = "grey50")+
  scale_fill_manual(values=pal,labels = c("1","2")) + 
  theme(text = element_text(size=15),
        axis.text.x = element_text(angle = 45, hjust = 1),
        #panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  geom_point(data = melted_miss_matrix[which(!is.na(melted_miss_matrix$count)),],
             aes(size = size_level), 
             color='lightpink',alpha=0.5) + 
  scale_size_manual(values = c(1,3,5,7),labels = c(expression(""<='11.8'),
                                                   expression(""<='13.0'),
                                                   expression(""<=14.4),
                                                   expression(""<='15.8')))+
  labs(fill = "Count",size = 'Avg Speed\n(m/s)') +
  geom_line(aes(y = 'CA'),color='red') +
  geom_vline(xintercept = 8.5, linetype = 2,color = 'indianred3',size=1.2)+
  geom_hline(yintercept = 8.5, linetype = 2,color = 'indianred3',size=1.2)+
  guides(color = guide_legend(order = 2),
         size = guide_legend(order = 1))


#Step5: Interpolation for each ST group-----
##for group 1.1/2.1/3.1----
#temporal gap is nearly stationary, thus:
#extend activities or add an activities between two trips
#update route information accordingly
actr$duration = as.numeric(actr$duration)
for (i in 1:length(actr$TripId)) {
  if(actr$subtype[i]=='MI'&actr$special[i]==0){
    if(sub("_.*", "", actr$label[i])=='1.1'|
       sub("_.*", "", actr$label[i])=='2.1'|
       sub("_.*", "", actr$label[i])=='3.1'){
      if(sub(".*_", "", actr$label[i])=='TT'){
        actr$subtype[i] = 'UA'
        actr$startlat[i] = actr$endlat[i-1]
        actr$startlon[i] = actr$endlon[i-1]
        actr$endlat[i] = actr$startlat[i+1]
        actr$endlon[i] = actr$startlon[i+1]
        route_update[[as.character(actr$UserId[i])]][
          as.character(actr$TripId[i])][[1]] = 
          data.frame(lat = actr$startlat[i], lon = actr$startlon[i])
        route_update[[as.character(actr$UserId[i])]][
          as.character(actr$TripId[i])][[1]][2,] = 
          c(actr$endlat[i],actr$endlon[i])
      }
      else if(sub(".*_", "", actr$label[i])=='AT'|
              sub(".*_", "", actr$label[i])=='TA'){
        if(actr$subtype[i-1]%in%activity_list){
          actr$endtime[i-1] = actr$endtime[i]
          actr$subtype[i] = 'REMOVE'
          actr$label[i-1] = paste('gap',actr$label[i],sep = '_')
          actr$label[i+1] = paste('gap',actr$label[i],sep = '_')
          if(distance84(actr$endlat[i-1],actr$endlon[i-1],
                        actr$startlat[i+1],actr$startlon[i+1])>0){
            actr$startlat[i+1] = actr$endlat[i-1]
            actr$startlon[i+1] = actr$endlon[i-1]
            route_update[[as.character(actr$UserId[i+1])]][
              as.character(actr$TripId[i+1])][[1]] = 
              rbind(c(actr$startlat[i+1],actr$startlon[i+1]),
                    route_update[[as.character(actr$UserId[i+1])]][
                      as.character(actr$TripId[i+1])][[1]])
          }
        }
        else if(actr$subtype[i+1]%in%activity_list){
          actr$starttime[i+1] = actr$starttime[i]
          actr$subtype[i] = 'REMOVE'
          actr$label[i-1] = paste('gap',actr$label[i],sep = '_')
          actr$label[i+1] = paste('gap',actr$label[i],sep = '_')
          if(distance84(actr$endlat[i-1],actr$endlon[i-1],
                        actr$startlat[i+1],actr$startlon[i+1])>0){
            actr$endlat[i-1] = actr$startlat[i+1]
            actr$endlon[i-1] = actr$startlon[i+1]
            route_update[[as.character(actr$UserId[i-1])]][
              as.character(actr$TripId[i-1])][[1]] = 
              rbind(route_update[[as.character(actr$UserId[i-1])]][
                as.character(actr$TripId[i-1])][[1]],
                c(actr$endlat[i-1],actr$endlon[i-1]))
          }
        }
      }
      else if(sub(".*_", "", actr$label[i])=='AA'){
        if(actr$duration[i]%%2 == 0){
          actr$endtime[i-1] = as.character(strptime(
            actr[i-1,"endtime"],format = "%Y-%m-%d %H:%M:%S") + 
              (actr$duration[i]/2*60))
          actr$starttime[i+1] = actr$endtime[i-1]
          actr$subtype[i] = 'REMOVE'
          actr$label[i-1] = paste('gap',actr$label[i],sep = '_')
          actr$label[i+1] = paste('gap',actr$label[i],sep = '_')
          if(distance84(actr$endlat[i-1],actr$endlon[i-1],
                        actr$startlat[i+1],actr$startlon[i+1])>0){
            actr$endlat[i-1] = midpoint(actr$endlat[i-1],
                                        actr$endlon[i-1],
                                        actr$startlat[i+1],
                                        actr$startlon[i+1])[1]
            actr$endlon[i-1] = midpoint(actr$endlat[i-1],
                                        actr$endlon[i-1],
                                        actr$startlat[i+1],
                                        actr$startlon[i+1])[2]
            actr$startlat[i+1] = midpoint(actr$endlat[i-1],
                                          actr$endlon[i-1],
                                          actr$startlat[i+1],
                                          actr$startlon[i+1])[1]
            actr$startlon[i+1] = midpoint(actr$endlat[i-1],
                                          actr$endlon[i-1],
                                          actr$startlat[i+1],
                                          actr$startlon[i+1])[2]
            route_update[[as.character(actr$UserId[i-1])]][
              as.character(actr$TripId[i-1])][[1]] = 
              rbind(route_update[[as.character(actr$UserId[i-1])]][
                as.character(actr$TripId[i-1])][[1]],
                c(actr$endlat[i-1],actr$endlon[i-1]))
            route_update[[as.character(actr$UserId[i+1])]][
              as.character(actr$TripId[i+1])][[1]] = 
              rbind(c(actr$startlat[i+1],actr$startlon[i+1]),
                    route_update[[as.character(actr$UserId[i+1])]][
                      as.character(actr$TripId[i+1])][[1]])
          }
        }
        else{
          if(abs(actr$speed[i]-actr$speed[i-1])<=
             abs(actr$speed[i]-actr$speed[i+1])){
            actr$endtime[i-1] = as.character(strptime(
              actr[i-1,"endtime"],format = "%Y-%m-%d %H:%M:%S") + 
                ((actr$duration[i]%/%2+1)*60))
            actr$starttime[i+1] = actr$endtime[i-1]
            actr$subtype[i] = 'REMOVE'
            actr$label[i-1] = paste('gap',actr$label[i],sep = '_')
            actr$label[i+1] = paste('gap',actr$label[i],sep = '_')
            if(distance84(actr$endlat[i-1],actr$endlon[i-1],
                          actr$startlat[i+1],actr$startlon[i+1])>0){
              actr$endlat[i-1] = midpoint(actr$endlat[i-1],
                                          actr$endlon[i-1],
                                          actr$startlat[i+1],
                                          actr$startlon[i+1])[1]
              actr$endlon[i-1] = midpoint(actr$endlat[i-1],
                                          actr$endlon[i-1],
                                          actr$startlat[i+1],
                                          actr$startlon[i+1])[2]
              actr$startlat[i+1] = midpoint(actr$endlat[i-1],
                                            actr$endlon[i-1],
                                            actr$startlat[i+1],
                                            actr$startlon[i+1])[1]
              actr$startlon[i+1] = midpoint(actr$endlat[i-1],
                                            actr$endlon[i-1],
                                            actr$startlat[i+1],
                                            actr$startlon[i+1])[2]
              route_update[[as.character(actr$UserId[i-1])]][
                as.character(actr$TripId[i-1])][[1]] = 
                rbind(route_update[[as.character(actr$UserId[i-1])]][
                  as.character(actr$TripId[i-1])][[1]],
                  c(actr$endlat[i-1],actr$endlon[i-1]))
              route_update[[as.character(actr$UserId[i+1])]][
                as.character(actr$TripId[i+1])][[1]] = 
                rbind(c(actr$startlat[i+1],actr$startlon[i+1]),
                      route_update[[as.character(actr$UserId[i+1])]][
                        as.character(actr$TripId[i+1])][[1]])
            }
          }
          else{
            actr$endtime[i-1] = as.character(strptime(
              actr[i-1,"endtime"],format = "%Y-%m-%d %H:%M:%S") + 
                (actr$duration[i]%/%2*60))
            actr$starttime[i+1] = actr$endtime[i-1]
            actr$subtype[i] = 'REMOVE'
            actr$label[i-1] = paste('gap',actr$label[i],sep = '_')
            actr$label[i+1] = paste('gap',actr$label[i],sep = '_')
            if(distance84(actr$endlat[i-1],actr$endlon[i-1],
                          actr$startlat[i+1],actr$startlon[i+1])>0){
              actr$endlat[i-1] = midpoint(actr$endlat[i-1],
                                          actr$endlon[i-1],
                                          actr$startlat[i+1],
                                          actr$startlon[i+1])[1]
              actr$endlon[i-1] = midpoint(actr$endlat[i-1],
                                          actr$endlon[i-1],
                                          actr$startlat[i+1],
                                          actr$startlon[i+1])[2]
              actr$startlat[i+1] = midpoint(actr$endlat[i-1],
                                            actr$endlon[i-1],
                                            actr$startlat[i+1],
                                            actr$startlon[i+1])[1]
              actr$startlon[i+1] = midpoint(actr$endlat[i-1],
                                            actr$endlon[i-1],
                                            actr$startlat[i+1],
                                            actr$startlon[i+1])[2]
              route_update[[as.character(actr$UserId[i-1])]][
                as.character(actr$TripId[i-1])][[1]] = 
                rbind(route_update[[as.character(actr$UserId[i-1])]][
                  as.character(actr$TripId[i-1])][[1]],
                  c(actr$endlat[i-1],actr$endlon[i-1]))
              route_update[[as.character(actr$UserId[i+1])]][
                as.character(actr$TripId[i+1])][[1]] = 
                rbind(c(actr$startlat[i+1],actr$startlon[i+1]),
                      route_update[[as.character(actr$UserId[i+1])]][
                        as.character(actr$TripId[i+1])][[1]])
            }
          }
        }
      }
    }
  }
}

##for group 1.2/2.2/3.2----
#temporal gap is at low-speed (walking), thus:
#extend walking trips or add a low-speed trip
#update route information accordingly
for (i in 1:length(actr$TripId)){
  if(actr$subtype[i]=='MI'&actr$special[i]==0){
    if(sub("_.*", "", actr$label[i])=='1.2'|
       sub("_.*", "", actr$label[i])=='2.2'|
       sub("_.*", "", actr$label[i])=='3.2'){
      if(actr$subtype[i-1]=='WL'&actr$subtype[i+1]=='WL'){
        if(actr$duration[i]%%2 == 0){
          actr$endtime[i-1] = as.character(strptime(
            actr[i-1,"endtime"],format = "%Y-%m-%d %H:%M:%S") + 
              (actr$duration[i]/2*60))
          actr$starttime[i+1] = actr$endtime[i-1]
          actr$subtype[i] = 'REMOVE'
          actr$label[i-1] = paste('gap',actr$label[i],sep = '_')
          actr$label[i+1] = paste('gap',actr$label[i],sep = '_')
          if(distance84(actr$endlat[i-1],actr$endlon[i-1],
                        actr$startlat[i+1],actr$startlon[i+1])>0){
            actr$endlat[i-1] = midpoint(actr$endlat[i-1],
                                        actr$endlon[i-1],
                                        actr$startlat[i+1],
                                        actr$startlon[i+1])[1]
            actr$endlon[i-1] = midpoint(actr$endlat[i-1],
                                        actr$endlon[i-1],
                                        actr$startlat[i+1],
                                        actr$startlon[i+1])[2]
            actr$startlat[i+1] = midpoint(actr$endlat[i-1],
                                          actr$endlon[i-1],
                                          actr$startlat[i+1],
                                          actr$startlon[i+1])[1]
            actr$startlon[i+1] = midpoint(actr$endlat[i-1],
                                          actr$endlon[i-1],
                                          actr$startlat[i+1],
                                          actr$startlon[i+1])[2]
            route_update[[as.character(actr$UserId[i-1])]][
              as.character(actr$TripId[i-1])][[1]] = 
              rbind(route_update[[as.character(actr$UserId[i-1])]][
                as.character(actr$TripId[i-1])][[1]],
                c(actr$endlat[i-1],actr$endlon[i-1]))
            route_update[[as.character(actr$UserId[i+1])]][
              as.character(actr$TripId[i+1])][[1]] = 
              rbind(c(actr$startlat[i+1],actr$startlon[i+1]),
                    route_update[[as.character(actr$UserId[i+1])]][
                      as.character(actr$TripId[i+1])][[1]])
          }
        }
        else{
          if(abs(actr$speed[i]-actr$speed[i-1])<=
             abs(actr$speed[i]-actr$speed[i+1])){
            actr$endtime[i-1] = as.character(strptime(
              actr[i-1,"endtime"],format = "%Y-%m-%d %H:%M:%S") + 
                ((actr$duration[i]%/%2+1)*60))
            actr$starttime[i+1] = actr$endtime[i-1]
            actr$subtype[i] = 'REMOVE'
            actr$label[i-1] = paste('gap',actr$label[i],sep = '_')
            actr$label[i+1] = paste('gap',actr$label[i],sep = '_')
            if(distance84(actr$endlat[i-1],actr$endlon[i-1],
                          actr$startlat[i+1],actr$startlon[i+1])>0){
              actr$endlat[i-1] = midpoint(actr$endlat[i-1],
                                          actr$endlon[i-1],
                                          actr$startlat[i+1],
                                          actr$startlon[i+1])[1]
              actr$endlon[i-1] = midpoint(actr$endlat[i-1],
                                          actr$endlon[i-1],
                                          actr$startlat[i+1],
                                          actr$startlon[i+1])[2]
              actr$startlat[i+1] = midpoint(actr$endlat[i-1],
                                            actr$endlon[i-1],
                                            actr$startlat[i+1],
                                            actr$startlon[i+1])[1]
              actr$startlon[i+1] = midpoint(actr$endlat[i-1],
                                            actr$endlon[i-1],
                                            actr$startlat[i+1],
                                            actr$startlon[i+1])[2]
              route_update[[as.character(actr$UserId[i-1])]][
                as.character(actr$TripId[i-1])][[1]] = 
                rbind(route_update[[as.character(actr$UserId[i-1])]][
                  as.character(actr$TripId[i-1])][[1]],
                  c(actr$endlat[i-1],actr$endlon[i-1]))
              route_update[[as.character(actr$UserId[i+1])]][
                as.character(actr$TripId[i+1])][[1]] = 
                rbind(c(actr$startlat[i+1],actr$startlon[i+1]),
                      route_update[[as.character(actr$UserId[i+1])]][
                        as.character(actr$TripId[i+1])][[1]])
            }
          }
          else{
            actr$endtime[i-1] = as.character(strptime(
              actr[i-1,"endtime"],format = "%Y-%m-%d %H:%M:%S") + 
                (actr$duration[i]%/%2*60))
            actr$starttime[i+1] = actr$endtime[i-1]
            actr$subtype[i] = 'REMOVE'
            actr$label[i-1] = paste('gap',actr$label[i],sep = '_')
            actr$label[i+1] = paste('gap',actr$label[i],sep = '_')
            if(distance84(actr$endlat[i-1],actr$endlon[i-1],
                          actr$startlat[i+1],actr$startlon[i+1])>0){
              actr$endlat[i-1] = midpoint(actr$endlat[i-1],
                                          actr$endlon[i-1],
                                          actr$startlat[i+1],
                                          actr$startlon[i+1])[1]
              actr$endlon[i-1] = midpoint(actr$endlat[i-1],
                                          actr$endlon[i-1],
                                          actr$startlat[i+1],
                                          actr$startlon[i+1])[2]
              actr$startlat[i+1] = midpoint(actr$endlat[i-1],
                                            actr$endlon[i-1],
                                            actr$startlat[i+1],
                                            actr$startlon[i+1])[1]
              actr$startlon[i+1] = midpoint(actr$endlat[i-1],
                                            actr$endlon[i-1],
                                            actr$startlat[i+1],
                                            actr$startlon[i+1])[2]
              route_update[[as.character(actr$UserId[i-1])]][
                as.character(actr$TripId[i-1])][[1]] = 
                rbind(route_update[[as.character(actr$UserId[i-1])]][
                  as.character(actr$TripId[i-1])][[1]],
                  c(actr$endlat[i-1],actr$endlon[i-1]))
              route_update[[as.character(actr$UserId[i+1])]][
                as.character(actr$TripId[i+1])][[1]] = 
                rbind(c(actr$startlat[i+1],actr$startlon[i+1]),
                      route_update[[as.character(actr$UserId[i+1])]][
                        as.character(actr$TripId[i+1])][[1]])
            }
          }
        }
      }
      else if(actr$subtype[i-1]=='WL'&actr$subtype[i+1]!='WL'){
        actr$endtime[i-1] = actr$endtime[i]
        actr$subtype[i] =  'REMOVE'
        actr$label[i-1] = paste('gap',actr$label[i],sep = '_')
        actr$label[i+1] = paste('gap',actr$label[i],sep = '_')
        if(distance84(actr$endlat[i-1],actr$endlon[i-1],
                      actr$startlat[i+1],actr$startlon[i+1])>0){
          actr$endlat[i-1] = actr$startlat[i+1]
          actr$endlon[i-1] = actr$startlon[i+1]
          route_update[[as.character(actr$UserId[i-1])]][
            as.character(actr$TripId[i-1])][[1]] = 
            rbind(route_update[[as.character(actr$UserId[i-1])]][
              as.character(actr$TripId[i-1])][[1]],
              c(actr$endlat[i-1],actr$endlon[i-1]))
        }
      }
      else if(actr$subtype[i-1]!='WL'&actr$subtype[i+1]=='WL'){
        actr$starttime[i+1] = actr$starttime[i]
        actr$subtype[i] = 'REMOVE'
        actr$label[i-1] = paste('gap',actr$label[i],sep = '_')
        actr$label[i+1] = paste('gap',actr$label[i],sep = '_')
        if(distance84(actr$endlat[i-1],actr$endlon[i-1],
                      actr$startlat[i+1],actr$startlon[i+1])>0){
          actr$startlat[i+1] = actr$endlat[i-1]
          actr$startlon[i+1] = actr$endlon[i-1]
          route_update[[as.character(actr$UserId[i+1])]][
            as.character(actr$TripId[i+1])][[1]] = 
            rbind(c(actr$startlat[i+1],actr$startlon[i+1]),
                  route_update[[as.character(actr$UserId[i+1])]][
                    as.character(actr$TripId[i+1])][[1]])
        }
      }
      else if(actr$subtype[i-1]!='WL'&actr$subtype[i+1]!='WL'){
        actr$subtype[i] = 'LS'
        actr$type[i] = 'TRIP'
        actr$startlat[i] = actr$endlat[i-1]
        actr$startlon[i] = actr$endlon[i-1]
        actr$endlat[i] = actr$startlat[i+1]
        actr$endlon[i] = actr$startlon[i+1]
        route_update[[as.character(actr$UserId[i])]][
          as.character(actr$TripId[i])][[1]] = 
          data.frame(lat = actr$startlat[i], lon = actr$startlon[i])
        route_update[[as.character(actr$UserId[i])]][
          as.character(actr$TripId[i])][[1]][2,] = 
          c(actr$endlat[i],actr$endlon[i])
      }
    }
  }
}

##for group 1.3/2.3/3.3----
#temporal gap is at low-speed (biking), thus:
#extend biking trips or add a low-speed trip
#update route information accordingly
for (i in 1:length(actr$TripId)){
  if(actr$subtype[i]=='MI'&actr$special[i]==0){
    if(sub("_.*", "", actr$label[i])=='1.3'|
       sub("_.*", "", actr$label[i])=='2.3'|
       sub("_.*", "", actr$label[i])=='3.3'){
      if(actr$subtype[i-1]=='BI'&actr$subtype[i+1]=='BI'){
        if(actr$duration[i]%%2 == 0){
          actr$endtime[i-1] = as.character(strptime(
            actr[i-1,"endtime"],format = "%Y-%m-%d %H:%M:%S") + 
              (actr$duration[i]/2*60))
          actr$starttime[i+1] = actr$endtime[i-1]
          actr$subtype[i] = 'REMOVE'
          actr$label[i-1] = paste('gap',actr$label[i],sep = '_')
          actr$label[i+1] = paste('gap',actr$label[i],sep = '_')
          if(distance84(actr$endlat[i-1],actr$endlon[i-1],
                        actr$startlat[i+1],actr$startlon[i+1])>0){
            actr$endlat[i-1] = midpoint(actr$endlat[i-1],
                                        actr$endlon[i-1],
                                        actr$startlat[i+1],
                                        actr$startlon[i+1])[1]
            actr$endlon[i-1] = midpoint(actr$endlat[i-1],
                                        actr$endlon[i-1],
                                        actr$startlat[i+1],
                                        actr$startlon[i+1])[2]
            actr$startlat[i+1] = midpoint(actr$endlat[i-1],
                                          actr$endlon[i-1],
                                          actr$startlat[i+1],
                                          actr$startlon[i+1])[1]
            actr$startlon[i+1] = midpoint(actr$endlat[i-1],
                                          actr$endlon[i-1],
                                          actr$startlat[i+1],
                                          actr$startlon[i+1])[2]
            route_update[[as.character(actr$UserId[i-1])]][
              as.character(actr$TripId[i-1])][[1]] = 
              rbind(route_update[[as.character(actr$UserId[i-1])]][
                as.character(actr$TripId[i-1])][[1]],
                c(actr$endlat[i-1],actr$endlon[i-1]))
            route_update[[as.character(actr$UserId[i+1])]][
              as.character(actr$TripId[i+1])][[1]] = 
              rbind(c(actr$startlat[i+1],actr$startlon[i+1]),
                    route_update[[as.character(actr$UserId[i+1])]][
                      as.character(actr$TripId[i+1])][[1]])
          }
        }
      }
      else if(actr$subtype[i+1]=='BI'){
        actr$starttime[i+1] = actr$starttime[i]
        actr$subtype[i] = 'REMOVE'
        actr$label[i-1] = paste('gap',actr$label[i],sep = '_')
        actr$label[i+1] = paste('gap',actr$label[i],sep = '_')
        if(distance84(actr$endlat[i-1],actr$endlon[i-1],
                      actr$startlat[i+1],actr$startlon[i+1])>0){
          actr$startlat[i+1] = actr$endlat[i-1]
          actr$startlon[i+1] = actr$endlon[i-1]
          route_update[[as.character(actr$UserId[i+1])]][
            as.character(actr$TripId[i+1])][[1]] = 
            rbind(c(actr$startlat[i+1],actr$startlon[i+1]),
                  route_update[[as.character(actr$UserId[i+1])]][
                    as.character(actr$TripId[i+1])][[1]])
        }
      }
      else{
        actr$subtype[i] = 'LS'
        actr$type[i] = 'TRIP'
        actr$startlat[i] = actr$endlat[i-1]
        actr$startlon[i] = actr$endlon[i-1]
        actr$endlat[i] = actr$startlat[i+1]
        actr$endlon[i] = actr$startlon[i+1]
        route_update[[as.character(actr$UserId[i])]][
          as.character(actr$TripId[i])][[1]] = 
          data.frame(lat = actr$startlat[i], lon = actr$startlon[i])
        route_update[[as.character(actr$UserId[i])]][
          as.character(actr$TripId[i])][[1]][2,] = 
          c(actr$endlat[i],actr$endlon[i])
      }
    }
  }
}

vehicle = c('CA','BU','RA','IV')
##for group 1.4/2.4/3.4----
#temporal gap is at high-speed, thus:
#extend vehicle-related trips or add a high-speed trip
#update route information accordingly
for (i in 1:length(actr$TripId)){
  if(actr$subtype[i]=='MI'&actr$special[i]==0){
    if(sub("_.*", "", actr$label[i])=='1.4'|
       sub("_.*", "", actr$label[i])=='2.4'|
       sub("_.*", "", actr$label[i])=='3.4'){
      if(actr$subtype[i-1]%in%vehicle &actr$subtype[i+1]%in%vehicle){
        if(actr$duration[i]%%2 == 0){
          actr$endtime[i-1] = as.character(
            strptime(actr[i-1,"endtime"],format = "%Y-%m-%d %H:%M:%S") + 
              (actr$duration[i]/2*60))
          actr$starttime[i+1] = actr$endtime[i-1]
          actr$subtype[i] = 'REMOVE'
          actr$label[i-1] = paste('gap',actr$label[i],sep = '_')
          actr$label[i+1] = paste('gap',actr$label[i],sep = '_')
          if(distance84(actr$endlat[i-1],actr$endlon[i-1],
                        actr$startlat[i+1],actr$startlon[i+1])>0){
            actr$endlat[i-1] = midpoint(actr$endlat[i-1],
                                        actr$endlon[i-1],
                                        actr$startlat[i+1],
                                        actr$startlon[i+1])[1]
            actr$endlon[i-1] = midpoint(actr$endlat[i-1],
                                        actr$endlon[i-1],
                                        actr$startlat[i+1],
                                        actr$startlon[i+1])[2]
            actr$startlat[i+1] = midpoint(actr$endlat[i-1],
                                          actr$endlon[i-1],
                                          actr$startlat[i+1],
                                          actr$startlon[i+1])[1]
            actr$startlon[i+1] = midpoint(actr$endlat[i-1],
                                          actr$endlon[i-1],
                                          actr$startlat[i+1],
                                          actr$startlon[i+1])[2]
            route_update[[as.character(actr$UserId[i-1])]][
              as.character(actr$TripId[i-1])][[1]] = 
              rbind(route_update[[as.character(actr$UserId[i-1])]][
                as.character(actr$TripId[i-1])][[1]],
                c(actr$endlat[i-1],actr$endlon[i-1]))
            route_update[[as.character(actr$UserId[i+1])]][
              as.character(actr$TripId[i+1])][[1]] = 
              rbind(c(actr$startlat[i+1],actr$startlon[i+1]),
                    route_update[[as.character(actr$UserId[i+1])]][
                      as.character(actr$TripId[i+1])][[1]])
          }
        }
        else{
          if(abs(actr$speed[i]-actr$speed[i-1])<=
             abs(actr$speed[i]-actr$speed[i+1])){
            actr$endtime[i-1] = as.character(strptime(
              actr[i-1,"endtime"],format = "%Y-%m-%d %H:%M:%S") + 
                ((actr$duration[i]%/%2+1)*60))
            actr$starttime[i+1] = actr$endtime[i-1]
            actr$subtype[i] = 'REMOVE'
            actr$label[i-1] = paste('gap',actr$label[i],sep = '_')
            actr$label[i+1] = paste('gap',actr$label[i],sep = '_')
            if(distance84(actr$endlat[i-1],actr$endlon[i-1],
                          actr$startlat[i+1],actr$startlon[i+1])>0){
              actr$endlat[i-1] = midpoint(actr$endlat[i-1],
                                          actr$endlon[i-1],
                                          actr$startlat[i+1],
                                          actr$startlon[i+1])[1]
              actr$endlon[i-1] = midpoint(actr$endlat[i-1],
                                          actr$endlon[i-1],
                                          actr$startlat[i+1],
                                          actr$startlon[i+1])[2]
              actr$startlat[i+1] = midpoint(actr$endlat[i-1],
                                            actr$endlon[i-1],
                                            actr$startlat[i+1],
                                            actr$startlon[i+1])[1]
              actr$startlon[i+1] = midpoint(actr$endlat[i-1],
                                            actr$endlon[i-1],
                                            actr$startlat[i+1],
                                            actr$startlon[i+1])[2]
              route_update[[as.character(actr$UserId[i-1])]][
                as.character(actr$TripId[i-1])][[1]] = 
                rbind(route_update[[as.character(actr$UserId[i-1])]][
                  as.character(actr$TripId[i-1])][[1]],
                  c(actr$endlat[i-1],actr$endlon[i-1]))
              route_update[[as.character(actr$UserId[i+1])]][
                as.character(actr$TripId[i+1])][[1]] = 
                rbind(c(actr$startlat[i+1],actr$startlon[i+1]),
                      route_update[[as.character(actr$UserId[i+1])]][
                        as.character(actr$TripId[i+1])][[1]])
            }
          }
          else{
            actr$endtime[i-1] = as.character(strptime(
              actr[i-1,"endtime"],format = "%Y-%m-%d %H:%M:%S") + 
                (actr$duration[i]%/%2*60))
            actr$starttime[i+1] = actr$endtime[i-1]
            actr$subtype[i] = 'REMOVE'
            actr$label[i-1] = paste('gap',actr$label[i],sep = '_')
            actr$label[i+1] = paste('gap',actr$label[i],sep = '_')
            if(distance84(actr$endlat[i-1],actr$endlon[i-1],
                          actr$startlat[i+1],actr$startlon[i+1])>0){
              actr$endlat[i-1] = midpoint(actr$endlat[i-1],
                                          actr$endlon[i-1],
                                          actr$startlat[i+1],
                                          actr$startlon[i+1])[1]
              actr$endlon[i-1] = midpoint(actr$endlat[i-1],
                                          actr$endlon[i-1],
                                          actr$startlat[i+1],
                                          actr$startlon[i+1])[2]
              actr$startlat[i+1] = midpoint(actr$endlat[i-1],
                                            actr$endlon[i-1],
                                            actr$startlat[i+1],
                                            actr$startlon[i+1])[1]
              actr$startlon[i+1] = midpoint(actr$endlat[i-1],
                                            actr$endlon[i-1],
                                            actr$startlat[i+1],
                                            actr$startlon[i+1])[2]
              route_update[[as.character(actr$UserId[i-1])]][
                as.character(actr$TripId[i-1])][[1]] = 
                rbind(route_update[[as.character(actr$UserId[i-1])]][
                  as.character(actr$TripId[i-1])][[1]],
                  c(actr$endlat[i-1],actr$endlon[i-1]))
              route_update[[as.character(actr$UserId[i+1])]][
                as.character(actr$TripId[i+1])][[1]] = 
                rbind(c(actr$startlat[i+1],actr$startlon[i+1]),
                      route_update[[as.character(actr$UserId[i+1])]][
                        as.character(actr$TripId[i+1])][[1]])
            }
          }
        }
      }
      else if(actr$subtype[i-1]%in%vehicle){
        actr$endtime[i-1] = actr$endtime[i]
        actr$subtype[i] = 'REMOVE'
        actr$label[i-1] = paste('gap',actr$label[i],sep = '_')
        actr$label[i+1] = paste('gap',actr$label[i],sep = '_')
        if(distance84(actr$endlat[i-1],actr$endlon[i-1],
                      actr$startlat[i+1],actr$startlon[i+1])>0){
          actr$endlat[i-1] = actr$startlat[i+1]
          actr$endlon[i-1] = actr$startlon[i+1]
          route_update[[as.character(actr$UserId[i-1])]][
            as.character(actr$TripId[i-1])][[1]] = 
            rbind(route_update[[as.character(actr$UserId[i-1])]][
              as.character(actr$TripId[i-1])][[1]],
              c(actr$endlat[i-1],actr$endlon[i-1]))
        }
      }
      else if(actr$subtype[i+1]%in%vehicle){
        actr$starttime[i+1] = actr$starttime[i]
        actr$subtype[i] = 'REMOVE'
        actr$label[i-1] = paste('gap',actr$label[i],sep = '_')
        actr$label[i+1] = paste('gap',actr$label[i],sep = '_')
        if(distance84(actr$endlat[i-1],actr$endlon[i-1],
                      actr$startlat[i+1],actr$startlon[i+1])>0){
          actr$startlat[i+1] = actr$endlat[i-1]
          actr$startlon[i+1] = actr$endlon[i-1]
          route_update[[as.character(actr$UserId[i+1])]][
            as.character(actr$TripId[i+1])][[1]] = 
            rbind(c(actr$startlat[i+1],actr$startlon[i+1]),
                  route_update[[as.character(actr$UserId[i+1])]][
                    as.character(actr$TripId[i+1])][[1]])
        }
      }
      else{
        actr$subtype[i] = 'HS'
        actr$type[i] = 'TRIP'
        actr$startlat[i] = actr$endlat[i-1]
        actr$startlon[i] = actr$endlon[i-1]
        actr$endlat[i] = actr$startlat[i+1]
        actr$endlon[i] = actr$startlon[i+1]
        route_update[[as.character(actr$UserId[i])]][
          as.character(actr$TripId[i])][[1]] = 
          data.frame(lat = actr$startlat[i], lon = actr$startlon[i])
        route_update[[as.character(actr$UserId[i])]][
          as.character(actr$TripId[i])][[1]][2,] = 
          c(actr$endlat[i],actr$endlon[i])
      }
    }
  }
}



################################################
#Interpolate Temporal Overlaps
################################################

################################################
#Interpolate Temporal Within
################################################