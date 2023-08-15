## Generate a uniform grid of points on a sphere 
## (Uniformity increases with increasing number of points)
## 
## R script implementation of https://www.cmu.edu/biolphys/deserno/pdf/sphere_equi.pdf
##
## n: number of points to generate (may not match output number exactly)
## sf: logical indicating if an sf point object should be returned (default 
##     returns a matrix)


geo_grid_unif <- function(n = 100, sf = FALSE){
  coord1 <-  matrix(, nrow = n + 1000 , ncol = 2)
  a <- 4 * pi /n
  d <- sqrt(a)
  Mlat = round(pi / d)
  Dlat = pi/Mlat 
  Dlon = a/Dlat
  ncount <- 0
  for(m in 0:(Mlat-1) ) {
    lat = pi * (m + 0.5)/ Mlat
    Mlon = round(2 * pi *  sin(lat) / Dlon)
    for (n in 0:(Mlon-1)){
      lon = 2 * pi * n / Mlon
      coord1[ncount,] <- c( lon * 180 / pi -180,  lat * 180 / pi -90) 
      ncount = ncount + 1
      if( abs(lat * 180 / pi -90) > 90) {
        print(c(lon * 180 / pi,lat * 180 / pi -90,Mlon ,m , n))
      }
    }
  }
  
  pts <- na.omit(coord1)
  
  if( sf == TRUE){
    return(sf::st_as_sf(as.data.frame(pts), 
                        coords = c(1,2), crs = "EPSG:4326"))
  } else {
    return(pts)
  }
}

## Test for uniformity 
# stdvar <- \(x) {3* sd(x)/mean(x)}
# pts <- geo_grid_unif(n = 100, sf = TRUE)
# st_distance(pts) -> test
# apply(test,1, FUN = \(x) sort(x)[2]) |> stdvar()
