# Functions rotation.create(), latlon.to.xyz(), circle.create(), and rotate(), 
# originally created by Bill Huber on stackexchange at
# https://gis.stackexchange.com/a/251873/4490

deg2rad <- function(phi) phi * (pi / 180)
rad2deg <- function(phi) phi * (180 / pi)
#
# Create a 3X3 matrix to rotate the North Pole to latitude `phi`, longitude 0.
# Solution: A rotation is a linear map, and therefore is determined by its
#           effect on a basis.  This rotation does the following:
#           (0,0,1) -> (cos(phi), 0, sin(phi))  {North Pole (Z-axis)}
#           (0,1,0) -> (0,1,0)                  {Y-axis is fixed}
#           (1,0,0) -> (sin(phi), 0, -cos(phi)) {Destination of X-axis}
#
rotation.create <- function(phi, is.radians=FALSE) {
  if (!is.radians) phi <- deg2rad(phi)
  cos.phi <- cos(phi)
  sin.phi <- sin(phi)
  matrix(c(sin.phi, 0, -cos.phi, 0, 1, 0, cos.phi, 0, sin.phi), 3)
}
#
# Convert between geocentric and spherical coordinates for a unit sphere.
# Assumes `latlon` in degrees.  It may be a 2-vector or a 2-row matrix.
# Returns an array with three rows for x,y,z.
#
latlon.to.xyz <- function(latlon) {
  latlon <- deg2rad(latlon)
  latlon <- matrix(latlon, nrow=2)
  cos.phi <- cos(latlon[1,])
  sin.phi <- sin(latlon[1,])
  cos.lambda <- cos(latlon[2,])
  sin.lambda <- sin(latlon[2,])
  rbind(x = cos.phi * cos.lambda,
        y = cos.phi * sin.lambda,
        z = sin.phi)
}
xyz.to.latlon <- function(xyz) {
  xyz <- matrix(xyz, nrow=3) 
  rad2deg(rbind(phi=atan2(xyz[3,], sqrt(xyz[1,]^2 + xyz[2,]^2)),
                lambda=atan2(xyz[2,], xyz[1,])))
}
#
# Create a circle of radius `r` centered at the North Pole, oriented positively.
# `r` is measured relative to the sphere's radius `R`.  For the authalic Earth,
# r==1 corresponds to 6,371,007.2 meters.
#
# `resolution` is the number of vertices to use in a polygonal approximation.
# The first and last vertex will coincide.
#
circle.create <- function(r=radius, resolution=360, R=6371007.2) {
  phi <- pi/2 - r / R                       # Constant latitude of the circle
  resolution <- max(1, ceiling(resolution)) # Assures a positive integer
  rad2deg(rbind(phi=rep(phi, resolution+1),
                lambda=seq(0, 2*pi, length.out = resolution+1)))
}
#
# Rotate around the y-axis, sending the North Pole to `phi`; then
# rotate around the new North Pole by `lambda`.
# Output is in geographic (spherical) coordinates, but input points may be
# in Earth-centered Cartesian or geographic.
# No effort is made to clamp longitudes to a 360 degree range.  This can 
# facilitate later computations.  Clamping is easily done afterwards if needed:
# reduce the longitude modulo 360 degrees.
#
rotate <- function(p, phi, lambda, is.geographic=FALSE) {
  if (is.geographic) p <- latlon.to.xyz(p)
  a <- rotation.create(phi)   # First rotation matrix
  q <- xyz.to.latlon(a %*% p) # Rotate the XYZ coordinates
  q + c(0, lambda)            # Second rotation
}

# Main function (Manny Gimond)

geo_buf_geodesic <- function(p, radius = 5000, seg=30, sphere.r = 6371007.2 ){
  library(sf)
  
  # Check for errors
  if( !("sf" %in% class(p)) ) stop('p is not an sf object.')
  if(is.null(st_crs(p, parameters = TRUE)$IsGeographic)) 
    stop('p does not have a defined coordinate system')
  if( !st_crs(p, parameters = TRUE)$IsGeographic)
    stop('p is not in a geographic coordinate system.')
  # Can't have radius greater than 1/4 of earth's circumference
  # or about 10,000,000 meters
  if( radius >=10000000)
    stop('Radius needs to be less than 10,000,000 meters.')  
  
  resolution <- seg * 4
  
  # Generate polar buffer
  p.0 <- circle.create(radius, resolution=resolution, R=sphere.r) 
  p.0 <- latlon.to.xyz(p.0)
  # Initialize variable 
  geo_circ <- p[0,]

  for(i in 1: nrow(p)){
    center <- st_coordinates(p[i,])
    circle <- rotate(p.0, center[2], center[1])
    poly1.crd <- round(t(circle)[,c(2,1)], 7)
    poly1.geom <- st_polygon(list(poly1.crd))
    poly.sfc <- st_as_sf(st_sfc( list(poly1.geom), crs = "EPSG:4326" ))
    geo_circ <- rbind(geo_circ, poly.sfc)
  }
  return(geo_circ)
}

