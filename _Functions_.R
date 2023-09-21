distance84 = function(lat1,long1,lat2,long2){
  #r = 6378.137
  r = 6371.001
  x1 = lat1 * pi / 180
  x2 = lat2 * pi / 180
  y1 = long1 * pi / 180
  y2 = long2 * pi / 180
  dx = abs(x1 - x2)
  dy = abs(y1 - y2)
  p = 2 * asin(sqrt((sin(dx/2)**2) + cos(x1)*cos(x2)*(sin(dy/2)**2)))
  return (round(r *p *10000)/10000)
}

midpoint = function(lat1,lon1,lat2,lon2){
  point1 = st_point(c(lon1,lat1))
  point2 = st_point(c(lon2,lat2))
  new_point1 = st_sfc(point1, point2, 
                      crs = "+proj=longlat +datum=WGS84 +no_defs")
  new_point2 = st_transform(new_point1,26915)
  point = data.frame(c(unlist(new_point2)[1],unlist(new_point2)[3]),
                     c(unlist(new_point2)[2],unlist(new_point2)[4]))
  names(point) = c('lon','lat')
  midpoint = st_point(c((point$lon[1] + point$lon[2])/2,
                        (point$lat[1] + point$lat[2])/2))
  midpoint = st_sfc(midpoint, 
                    crs = "+proj=utm +zone=15 +datum=NAD83 +units=m +no_defs")
  midpoint = st_transform(midpoint,4326)
  return(c(unlist(midpoint)[2],unlist(midpoint)[1]))
}

natransfer<-function(x){
  if(is.na(x)==TRUE) 
    return (0) else 
      return(x)
}

