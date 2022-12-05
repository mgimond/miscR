##
## Function generates geometrically correct buffers from an sf points object
## stored in a geographic coordinate system.
## Output consists of an sf polygon object in a WGS84 coordinate
## system.
## ESRI's World Azimuthal Equidistant projection is used to generate the circles
##
## Error in area measurement is within 0.05% for 5 km buffer
## Error in area measurement is within 0.1% for 500 km buffer
## Error in area measurement is within 5% for 5000 km buffer
##
## p: point sf object in a geographic coordinate system (i.e. lon/lat values)
## seg: number of line segments for a quarter of the circle. (e.g. a seg = 3
##      will generate a circle with 12 line segments)
## radius: buffer radius in meters
## 

library(sf)

# Define equidistant projection in WKT format
geo_buf_equi <- function(x0 = 0, y0 = 45) {
  paste('PROJCS["World_Azimuthal_Equidistant",
       GEOGCS["WGS 84",
              DATUM["WGS_1984",
                    SPHEROID["WGS 84",6378137,298.257223563,
                             AUTHORITY["EPSG","7030"]],
                    AUTHORITY["EPSG","6326"]],
              PRIMEM["Greenwich",0],
              UNIT["Degree",0.0174532925199433]],
       PROJECTION["Azimuthal_Equidistant"],
       PARAMETER["latitude_of_center", ',y0,' ],
       PARAMETER["longitude_of_center", ',x0, '  ],
       PARAMETER["false_easting",0],
       PARAMETER["false_northing",0],
       UNIT["metre",1,
            AUTHORITY["EPSG","9001"]],
       AXIS["Easting",EAST],
       AXIS["Northing",NORTH],
       AUTHORITY["ESRI","54032"]]')
}

# Function that projects points to equidistant CS, then
# generates "true" circle
geo_buf <- function(p, radius = 5000, seg = 30){
  if( !("sf" %in% class(p)) ) stop('p is not an sf object.')
  if(is.null(st_crs(p, parameters = TRUE)$IsGeographic)) 
    stop('p does not have a defined coordinate system')
  if( !st_crs(p, parameters = TRUE)$IsGeographic)
    stop('p is not in a geographic coordinate system.')
  geo_circ <- p[0,]
  for(i in 1: nrow(p)){
    center <- st_coordinates(p[i,])
    buf <- st_buffer(st_transform(p[i,], 
                                  crs = geo_buf_equi(center[1], center[2])), 
                     dist = radius,
                     nQuadSegs = seg)
    geo_circ <- rbind(geo_circ, st_transform(st_sf(buf), crs = "EPSG:4326"))
  }
  return(geo_circ)
}

