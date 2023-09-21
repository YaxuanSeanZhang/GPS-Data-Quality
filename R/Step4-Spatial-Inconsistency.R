################################################
#Define Spatial Inconsistency
################################################
#Step1: Create spatial gap list-----
spatialdistance = data.frame(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
colnames(spatialdistance)<-c("UserId","from","to","startlat","endlat",
                             "startlon","endlon","TripId",
                             "formerspeed",'laterspeed','formerendtime',
                             'nextstarttime','same','special')
j = 1
for (i in 1:(length(actr$TripId)-1)) {     
  if((actr$UserId[i] == actr$UserId[i+1])
     &(!(actr$label[i]%in% spatial_skip))
     & (!(actr$label[i+1]%in% spatial_skip))){
    if((actr$label[i]%in% spatial_skip_partial & 
        (!(actr$label[i+1]%in% spatial_skip_partial)) 
        & actr$label[i+1]!= "GAP + SAME")|
       ((!(actr$label[i]%in% spatial_skip_partial)) & 
        actr$label[i+1]%in% spatial_skip_partial& 
        actr$label[i]!= "GAP + SAME") |
       ((!(actr$label[i]%in% spatial_skip_partial))& 
        (!(actr$label[i+1]%in% spatial_skip_partial))) |
       (actr$TripId[i]%in%before_partial)){
      spatialdistance$UserId[j] = as.character(actr$UserId[i])
      spatialdistance$from[j] = as.character(actr$subtype[i])
      spatialdistance$to[j] = as.character(actr$subtype[i+1])
      spatialdistance$formerspeed[j] = actr$speed[i]
      spatialdistance$laterspeed[j] = actr$speed[i+1]
      spatialdistance$TripId[j] = actr$TripId[i]
      spatialdistance$startlat[j] = actr$endlat[i]
      spatialdistance$startlon[j] = actr$endlon[i]
      spatialdistance$endlat[j] = actr$startlat[i+1]
      spatialdistance$endlon[j] = actr$startlon[i+1]
      spatialdistance$formerendtime[j] = actr$endtime[i]
      spatialdistance$nextstarttime[j] = actr$starttime[i+1]
      spatialdistance$same[j] = actr$same[i]
      spatialdistance$special[j] = actr$special[i]
      spatialdistance <- rbind(spatialdistance,NA)
      j = j+1
    }
  }
}
spatialdistance<-na.omit(spatialdistance)

spatialdistance[ ,"distance"] <- NA
spatialdistance$distance <- distance84(spatialdistance$startlat,
                                       spatialdistance$startlon,
                                       spatialdistance$endlat,
                                       spatialdistance$endlon)


#Step2: Spatial gap classification-----
#pick the best fitted model based on BIC value for classification

#mixture model fitting
#gaussian
mmNO1sp <- gamlssMX(spatialdistance$distance~1, family=NO, K=1) #fail
mmNO2sp <- gamlssMX(spatialdistance$distance~1, family=NO, K=2) #fail
mmNO3sp <- gamlssMX(spatialdistance$distance~1, family=NO, K=3) #fail
mmNO4sp <- gamlssMX(spatialdistance$distance~1, family=NO, K=4) #fail
mmNO5sp <- gamlssMX(spatialdistance$distance~1, family=NO, K=5)
mmNO6sp <- gamlssMX(spatialdistance$distance~1, family=NO, K=6) 

#inverse gaussian
mmIG1sp <- gamlssMX(spatialdistance$distance~1, family=IG, K=1)
mmIG2sp <- gamlssMX(spatialdistance$distance~1, family=IG, K=2)
mmIG3sp <- gamlssMX(spatialdistance$distance~1, family=IG, K=3)
mmIG4sp <- gamlssMX(spatialdistance$distance~1, family=IG, K=4)
mmIG5sp <- gamlssMX(spatialdistance$distance~1, family=IG, K=5)

#log-normall
mmLOGNO1sp <- gamlssMX(spatialdistance$distance~1, family=LOGNO2, K=1)
mmLOGNO2sp <- gamlssMX(spatialdistance$distance~1, family=LOGNO2, K=2)
mmLOGNO3sp <- gamlssMX(spatialdistance$distance~1, family=LOGNO2, K=3)
mmLOGNO4sp <- gamlssMX(spatialdistance$distance~1, family=LOGNO2, K=4)
mmLOGNO5sp <- gamlssMX(spatialdistance$distance~1, family=LOGNO2, K=5)

#inverse gamma
mmIGAMMA1sp <- gamlssMX(spatialdistance$distance~1, family=IGAMMA, K=1)
mmIGAMMA2sp <- gamlssMX(spatialdistance$distance~1, family=IGAMMA, K=2)
mmIGAMMA3sp <- gamlssMX(spatialdistance$distance~1, family=IGAMMA, K=3)
mmIGAMMA4sp <- gamlssMX(spatialdistance$distance~1, family=IGAMMA, K=4)
mmIGAMMA5sp <- gamlssMX(spatialdistance$distance~1, family=IGAMMA, K=4)

#reverse generalized inverse gaussian
mmGIG1sp <- gamlssMXfits(n =5, spatialdistance$distance~1, family=GIG, K=1)
mmGIG2sp <- gamlssMXfits(n =5, spatialdistance$distance~1, family=GIG, K=2)
mmGIG3sp <- gamlssMXfits(n =5, spatialdistance$distance~1, family=GIG, K=3)
mmGIG4sp <- gamlssMXfits(n =5, spatialdistance$distance~1, family=GIG, K=4)
mmGIG5sp <- gamlssMXfits(n =5, spatialdistance$distance~1, family=GIG, K=5)

#reverse gumbel
mmRG1sp <- gamlssMX(spatialdistance$distance~1, family=RG, K=1)
mmRG2sp <- gamlssMX(spatialdistance$distance~1, family=RG, K=2)
mmRG3sp <- gamlssMX(spatialdistance$distance~1, family=RG, K=3)
mmRG4sp <- gamlssMX(spatialdistance$distance~1, family=RG, K=4)
mmRG5sp <- gamlssMX(spatialdistance$distance~1, family=RG, K=5)
mmRG6sp <- gamlssMX(spatialdistance$distance~1, family=RG, K=6)
mmRG7sp <- gamlssMX(spatialdistance$distance~1, family=RG, K=7)

#exponential
mmEXP1sp <- gamlssMX(spatialdistance$distance~1, family=EXP, K=1)
mmEXP2sp <- gamlssMX(spatialdistance$distance~1, family=EXP, K=2)
mmEXP3sp <- gamlssMX(spatialdistance$distance~1, family=EXP, K=3)
mmEXP4sp <- gamlssMX(spatialdistance$distance~1, family=EXP, K=4)
mmEXP5sp <- gamlssMX(spatialdistance$distance~1, family=EXP, K=5)
mmEXP6sp <- gamlssMX(spatialdistance$distance~1, family=EXP, K=6)

#BIC plot for evaluation
BIC_s = data.frame(NA,NA)

BIC_s[1,] = c(mmRG1sp$sbc,NA)
BIC_s[2,] = c(mmRG2sp$sbc,NA)
BIC_s[3,] = c(mmRG3sp$sbc,NA)
BIC_s[4,] = c(mmRG4sp$sbc,NA)
BIC_s[5,] = c(mmRG5sp$sbc,mmNO5sp$sbc)
BIC_s[6,] = c(NA,mmNO6sp$sbc)

BIC_s = cbind(c(1,2,3,4,5,6),BIC_s)
colnames(BIC_s) = c('num_of_component', 'Reverse_Gumbel', 'Gaussian')

cols    <- c( "Reverse_Gumbel" = "lemonchiffon4", "Gaussian" = "forestgreen")
shapes     <- c( "Reverse_Gumbel" = 16, "Gaussian" = 15)

ggplot(data=BIC_s) +
  geom_point(aes(x=as.factor(num_of_component), y=Reverse_Gumbel,
                 color = 'Reverse_Gumbel',shape = 'Reverse_Gumbel'),size=3) + 
  geom_point(aes(x=as.factor(num_of_component), y=Gaussian,
                 color = 'Gaussian',shape = 'Gaussian'),size=3) + 
  geom_line(aes(x=as.factor(num_of_component), y=Reverse_Gumbel,group = 1),
            color = 'lemonchiffon4',size = 1) +
  geom_line(aes(x=as.factor(num_of_component), y=Gaussian,group = 1),
            color = 'forestgreen',size = 1) +
  scale_color_manual(name = "Model", breaks = c('Reverse_Gumbel','Gaussian'), 
                     values = cols,labels = c('Reverse_Gumbel','Gaussian'))+
  labs(x = "Number of Compoents",y = 'BIC',colour = "Model") + 
  scale_shape_manual(name = "Model",breaks = c('Reverse_Gumbel','Gaussian'), 
                     values = shapes,labels = c('Reverse_Gumbel','Gaussian'))+
  theme(legend.position = c(0.85, 0.75)) 


s_hist <- hist(spatialdistance$distance, xlim=c(0,0.02),
               breaks = seq(0, 16, by=0.001),xlab = 'distance_gap')
inXs = s_hist$mids 
inYs = s_hist$density
fit_datas <- data.frame(inXs, inYs)

fits1 = data.frame(distance = fit_datas$inXs, 
                   prob = 0.4499292 * dRG(
                     fit_datas$inXs, mu=0.002364, sigma=exp(-5.919)))
fits2 = data.frame(distance = fit_datas$inXs, 
                   prob = 0.227522 * dRG(
                     fit_datas$inXs, mu=0.01898, sigma=exp(-4.396)))
fits3 = data.frame(distance = fit_datas$inXs, 
                   prob = 0.1685598 * dRG(
                     fit_datas$inXs, mu=0.09195, sigma=exp(-2.897)))
fits4 = data.frame(distance= fit_datas$inXs, 
                   prob = 0.1212747 * dRG(
                     fit_datas$inXs, mu=0.4104, sigma=exp(-1.43)))
fits5 = data.frame(distance = fit_datas$inXs, 
                   prob = 0.03271422 * dRG(
                     fit_datas$inXs, mu=1.454, sigma=exp(-0.1219)))
fits = data.frame(distance = fit_datas$inXs, 
                  prob = dMX(fit_datas$inXs, 
                             mu=list(0.002364,0.01898,0.09195,0.4104,1.454), 
                             sigma=list(exp(-5.919),exp(-4.396),exp(-2.897),
                                        exp(-1.43),exp(-0.1219)), 
                             pi = list(0.4499292, 0.227522, 
                                       0.1685598, 0.1212747, 0.03271422),
                             family=list("RG","RG","RG","RG","RG")))


#classification result for paper
ggplot(spatialdistance, aes(x=distance)) + 
  geom_histogram(aes(y=(..density..)), position="identity",binwidth = 0.002) + 
  xlim(c(0, 0.3)) + 
  labs(x = "distance gap (0-0.3km)",y = 'density') + 
  geom_line(data = fits1,aes(x=distance, y=prob),
            color = 'forestgreen',size = 0.5) +
  geom_line(data = fits2,aes(x=distance, y=prob),
            color = 'lightpink3',size = 0.5) +
  geom_line(data = fits3,aes(x=distance, y=prob),
            color = 'goldenrod1',size = 0.5) +
  geom_line(data = fits4,aes(x=distance, y=prob),
            color = 'lemonchiffon4',size = 0.5) +
  geom_line(data = fits5,aes(x=distance, y=prob),
            color = 'deepskyblue3',size = 0.5) +
  geom_line(data = fits,aes(x=distance, y=prob),
            color = 'red',size = 1) 

#get spatial gap cut off value
##cut off value1
for (i in seq(0,16,0.001)) {
  if(0.4499292 * dRG(i, mu=0.002364, sigma=exp(-5.919)) < 0.227522 * 
     dRG(i, mu=0.01898, sigma=exp(-4.396))){
    print(i)
    break
  }
}  #12m

##cut off value2
##cut off value3
##cut off value4


################################################
#Interpolate Spatial Gaps
################################################
#Step1: Visual Exploration (Transition Matrix Plot)-----
#get melted_matrix
melted_matrix_spatial = function(miss){
  #initialization
  miss_matrix = data.frame(matrix(0, nrow = 16, ncol = 16))
  colnames(miss_matrix) = c('HO','WO','ED','PB','EO','LR','SH','OA',
                            'CA','BU','RA','IV','BI','WL','WI','OT')
  rownames(miss_matrix) = c('HO','WO','ED','PB','EO','LR','SH','OA',
                            'CA','BU','RA','IV','BI','WL','WI','OT')
  
  melted_miss_matrix = melt(as.matrix(miss_matrix))
  melted_miss_matrix = cbind(melted_miss_matrix,
                             data.frame(matrix(NaN, nrow = 256, ncol = 3)))
  colnames(melted_miss_matrix) = c('from','to','count','ave_dist',
                                   'max_dist','min_dist')
  #calculate
  for (i in 1:length(melted_miss_matrix$from)) {
    attribute = NA
    for(j in 1:length(miss$from)){
      if(melted_miss_matrix$from[i]==miss$from[j]&
         melted_miss_matrix$to[i]==miss$to[j]){
        melted_miss_matrix$count[i] = melted_miss_matrix$count[i] + 1
        attribute=c(attribute,miss$distance[j])
      }
    }
    attribute = attribute[-1]
    melted_miss_matrix$ave_dist[i] = mean(attribute)
    melted_miss_matrix$max_dist[i] = max(attribute)
    melted_miss_matrix$min_dist[i] = min(attribute)
  }
  return(melted_miss_matrix[order(melted_miss_matrix$count,decreasing = TRUE),])
}

#data preparation
melted_miss_matrix0 = melted_matrix_spatial(spatialdistance)
melted_miss_matrix1 = melted_matrix_spatial(spatialdistance[
  which(spatialdistance$distance<0.012),])
melted_miss_matrix2 = melted_matrix_spatial(spatialdistance[
  which(spatialdistance$distance>=0.012&spatialdistance$distance<0.057),])
melted_miss_matrix3 = melted_matrix_spatial(spatialdistance[
  which(spatialdistance$distance>=0.057&spatialdistance$distance<0.258),])
melted_miss_matrix4 = melted_matrix_spatial(spatialdistance[
  which(spatialdistance$distance>=0.258&spatialdistance$distance<1.276),])
melted_miss_matrix5 = melted_matrix_spatial(spatialdistance[
  which(spatialdistance$distance>=1.276),])

melted_miss_matrix = melted_miss_matrix5

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
melted_miss_matrix[order(melted_miss_matrix$count,decreasing = TRUE),]

pal <- wes_palette("Zissou1", 5, type = "discrete")
size_level = cut(melted_miss_matrix$ave_dist,
                 quantile(melted_miss_matrix$ave_dist[
                   which(!is.na(melted_miss_matrix$count))]),
                 include.lowest = TRUE)
size_level=size_level[!is.na(size_level)]

ggplot(data = melted_miss_matrix, 
       aes(x=to, y=from, fill=cut(count,c(0,4,9,49,99,162)))) + 
  xlab("to") + ylab("from") +  geom_tile(colour = "grey50")+
  scale_fill_manual(values=pal,labels = c("1-4", "5-9","10-49","50-99"
                                          ,"100-162"),drop = FALSE) + 
  theme(text = element_text(size=15),
        axis.text.x = element_text(angle = 45, hjust = 1),
        #panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  geom_point(data = melted_miss_matrix[which(!is.na(melted_miss_matrix$count)),],
             aes(size = size_level),
             color='lightpink',alpha=0.5) + 
  scale_size_manual(values = c(1,3,5,7),
                    labels = c(expression(""<=1.58),
                               expression(""<=2.21),
                               expression(""<='2.70'),
                               expression(""<=15.8)))+
  labs(fill = "Count",size = 'Avg Distance\n(m/s)') +
  geom_vline(xintercept = 8.5, linetype = 2,color = 'indianred3',size=1.2)+
  geom_hline(yintercept = 8.5, linetype = 2,color = 'indianred3',size=1.2)+
  guides(color = guide_legend(order = 2),
         size = guide_legend(order = 1))


#Step2: Interpolation for each group-----
##for small spatial gaps (within 258m): ----
#due to gps accuracy, thus, adopt midpoint as updated location
for (j in 1:length(actr$UserId)) {
  if(actr$TripId[j]%in%spatial_list){
    if(distance84(actr$endlat[j],actr$endlon[j],
                  actr$startlat[j+1],actr$startlon[j+1])>0 &
       distance84(actr$endlat[j],actr$endlon[j],
                  actr$startlat[j+1],actr$startlon[j+1])<0.258){  
      if((actr$subtype[j] %in% activity_list & 
          actr$subtype[j+1] %in% activity_list)|
         (actr$subtype[j] %in% trip_list & 
          actr$subtype[j+1] %in% trip_list)){  #average
        actr$endlat[j] = midpoint(actr$endlat[j],actr$endlon[j],
                                  actr$startlat[j+1],actr$startlon[j+1])[1]
        actr$endlon[j] = midpoint(actr$endlat[j],actr$endlon[j],
                                  actr$startlat[j+1],actr$startlon[j+1])[2]
        actr$startlat[j+1] = actr$endlat[j]
        actr$startlon[j+1] = actr$endlon[j]
        route_update[[as.character(actr$UserId[j])]][
          as.character(actr$TripId[j])][[1]]= rbind(
            route_update[[as.character(actr$UserId[j])]][
              as.character(actr$TripId[j])][[1]],c(actr[j,'endlat'],
                                                   actr[j,'endlon']))
        route_update[[as.character(actr$UserId[j+1])]][
          as.character(actr$TripId[j+1])][[1]]= rbind(
            c(actr[j+1,'startlat'],actr[j+1,'startlon']),
            route_update[[as.character(actr$UserId[j+1])]][
              as.character(actr$TripId[j+1])][[1]])
      }
      else if(actr$subtype[j] %in% activity_list & 
              actr$subtype[j+1] %in% trip_list){ #ac to tr  -- extend trip
        actr$startlat[j+1] = actr$endlat[j]
        actr$startlon[j+1] = actr$endlon[j]
        route_update[[as.character(actr$UserId[j+1])]][
          as.character(actr$TripId[j+1])][[1]]= rbind(
            c(actr[j+1,'startlat'],actr[j+1,'startlon']),
            route_update[[as.character(actr$UserId[j+1])]][
              as.character(actr$TripId[j+1])][[1]])
      }
      else if(actr$subtype[j] %in% trip_list & 
              actr$subtype[j+1] %in% activity_list){ #tr to ac -- extend trip
        actr$endlat[j] = actr$startlat[j+1]
        actr$endlon[j] = actr$startlon[j+1]
        route_update[[as.character(actr$UserId[j])]][
          as.character(actr$TripId[j])][[1]]= rbind(
            route_update[[as.character(actr$UserId[j])]][
              as.character(actr$TripId[j])][[1]],c(actr[j,'endlat'],
                                                   actr[j,'endlon']))
      }
    }
  }
}

##for big spatial gaps (over 258m): ----
#due to gps signal loss, cold start, thus, extend trip or add a trip
spatial_list = c(spatialdistance$TripId)
missing_trip = actr[1,]
for (j in 1:length(actr$UserId)) {
  if(actr$TripId[j]%in%spatial_list){
    if(distance84(actr$endlat[j],actr$endlon[j],
                  actr$startlat[j+1],actr$startlon[j+1])>=0.258){  
      if(actr$subtype[j] %in% activity_list & 
         actr$subtype[j+1] %in% activity_list){  #ac to ac -- add new trip
        missing_trip = rbind(missing_trip,
                             c(paste(actr$TripId[j],"MI",sep=''),actr$UserId[j],
                               actr$endtime[j],actr$starttime[j+1],'TRIP','MT',
                               0,0,0,0,0,0,
                               actr$endlat[j],actr$startlat[j+1],
                               actr$endlon[j],actr$startlon[j+1],
                               0,0,0,0,0,'Time Unknow Trip'))
        route_update[[as.character(actr$UserId[j])]][
          paste(actr$TripId[j],"MI",sep='')][[1]] = 
          data.frame(lat = actr$endlat[j], lon = actr$endlon[j])
        route_update[[as.character(actr$UserId[j])]][
          paste(actr$TripId[j],"MI",sep='')][[1]][2,] = 
          c(actr$startlat[j+1],actr$startlon[j+1])
      }
      else if(actr$subtype[j] %in% activity_list & 
              actr$subtype[j+1] %in% trip_list){ #ac to tr  -- extend trip
        actr$startlat[j+1] = actr$endlat[j]
        actr$startlon[j+1] = actr$endlon[j]
        route_update[[as.character(actr$UserId[j+1])]][
          as.character(actr$TripId[j+1])][[1]]= rbind(
            c(actr[j+1,'startlat'],actr[j+1,'startlon']),
            route_update[[as.character(actr$UserId[j+1])]][
              as.character(actr$TripId[j+1])][[1]])
        actr$label[j+1] = 'Signal Loss Trip (to)' #label extended trip
      }
      else if(actr$subtype[j] %in% trip_list & 
              actr$subtype[j+1] %in% activity_list){ #tr to ac -- extend trip
        actr$endlat[j] = actr$startlat[j+1]
        actr$endlon[j] = actr$startlon[j+1]
        route_update[[as.character(actr$UserId[j])]][
          as.character(actr$TripId[j])][[1]]= rbind(
            route_update[[as.character(actr$UserId[j])]][
              as.character(actr$TripId[j])][[1]],
            c(actr[j,'endlat'],actr[j,'endlon']))
        actr$label[j] = 'Signal Loss Trip (from)'  #label extended trip
      }
      else if(actr$subtype[j] %in% trip_list & 
              actr$subtype[j+1] %in% trip_list){  #tr to tr -- extend high speed trip
        if(actr$subtype[j]!='WL'&actr$subtype[j]!='WI'&
           (actr$subtype[j+1]=='WL'| actr$subtype[j+1]=='WI')){ 
          #the first trip is high speed, the second is walk/wait, extend first
          actr$endlat[j] = actr$startlat[j+1]
          actr$endlon[j] = actr$startlon[j+1]
          route_update[[as.character(actr$UserId[j])]][
            as.character(actr$TripId[j])][[1]]= rbind(
              route_update[[as.character(actr$UserId[j])]][
                as.character(actr$TripId[j])][[1]],c(actr[j,'endlat'],
                                                     actr[j,'endlon']))
          actr$label[j] = 'Signal Loss' #label extended trip
        }
        else if((actr$subtype[j]=='WL'| actr$subtype[j]=='WI')&
                actr$subtype[j+1]!='WL'& actr$subtype[j+1]!='WI'){ 
          #the second trip is high speed, the first is walk/wait, extend second
          actr$startlat[j+1] = actr$endlat[j]
          actr$startlon[j+1] = actr$endlon[j]
          route_update[[as.character(actr$UserId[j+1])]][
            as.character(actr$TripId[j+1])][[1]]= rbind(
              c(actr[j+1,'startlat'], actr[j+1,'startlon']),
              route_update[[as.character(actr$UserId[j+1])]][
                as.character(actr$TripId[j+1])][[1]])
          actr$label[j+1] = 'Signal Loss'  #label extended trip
        }
        else if(actr$subtype[j]=='WL'&actr$subtype[j+1]=='WI'){ #extend first walk
          actr$endlat[j] = actr$startlat[j+1]
          actr$endlon[j] = actr$startlon[j+1]
          route_update[[as.character(actr$UserId[j])]][
            as.character(actr$TripId[j])][[1]]= rbind(
              route_update[[as.character(actr$UserId[j])]][
                as.character(actr$TripId[j])][[1]], c(actr[j,'endlat'],
                                                      actr[j,'endlon']))
          actr$label[j] = 'Signal Loss'  #label extended trip
        }
        else if(actr$subtype[j]=='WI'&actr$subtype[j+1]=='WL'){ #extend second walk
          actr$startlat[j+1] = actr$endlat[j]
          actr$startlon[j+1] = actr$endlon[j]
          route_update[[as.character(actr$UserId[j+1])]][
            as.character(actr$TripId[j+1])][[1]]= rbind(
              c(actr[j+1,'startlat'],actr[j+1,'startlon']),
              route_update[[as.character(actr$UserId[j+1])]][
                as.character(actr$TripId[j+1])][[1]])
          actr$label[j+1] = 'Signal Loss' #label extended trip
        }
        else{  #average
          actr$endlat[j] = midpoint(actr$endlat[j],actr$endlon[j],
                                    actr$startlat[j+1],actr$startlon[j+1])[1]
          actr$endlon[j] = midpoint(actr$endlat[j],actr$endlon[j],
                                    actr$startlat[j+1],actr$startlon[j+1])[2]
          actr$startlat[j+1] = actr$endlat[j]
          actr$startlon[j+1] = actr$endlon[j]
          route_update[[as.character(actr$UserId[j])]][
            as.character(actr$TripId[j])][[1]]= rbind(
              route_update[[as.character(actr$UserId[j])]][
                as.character(actr$TripId[j])][[1]],c(actr[j,'endlat'],
                                                     actr[j,'endlon']))
          route_update[[as.character(actr$UserId[j+1])]][
            as.character(actr$TripId[j+1])][[1]]= rbind(
              c(actr[j+1,'startlat'],actr[j+1,'startlon']),
              route_update[[as.character(actr$UserId[j+1])]][
                as.character(actr$TripId[j+1])][[1]])
          actr$label[j+1] = 'Signal Loss'  #label the last trip
        }
      }
    }
  }
}

missing_trip = missing_trip[-1,]
missing_trip$startlat = as.numeric(missing_trip$startlat)
missing_trip$startlon = as.numeric(missing_trip$startlon)
missing_trip$endlat = as.numeric(missing_trip$endlat)
missing_trip$endlon = as.numeric(missing_trip$endlon)
missing_trip$distance = distance84(missing_trip$startlat,missing_trip$startlon,
                                   missing_trip$endlat,missing_trip$endlon)
actr = rbind(actr,missing_trip)
actr<-actr[order(actr$UserId,actr$starttime,actr$endtime),]