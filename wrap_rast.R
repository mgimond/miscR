##
## Function wraps a raster layer across the date line (i.e. the -180/180
## meridian). Function makes use of the terra package.
## Author: Manny Gimond
##
## Arguments:
##
## r: SpatRaster object in a geographic coordinate system.
## east: lower bound of the eastern section of the raster to append to the
##       western side of the raster (in degrees).
## west: upper bound of the western section of the raster to append to the
##       eastern side of the raster (in degrees).

wrap_rast <- function(r, east = 90, west = -90) {
  library(terra)
  if( class(r)[1] != "SpatRaster" ) 
    stop('r is not SpatRaster object.')
  if(!all.equal(c(-180,180), as.vector(ext(r)[1:2])))
    stop('East-west extents should cover the range [-180:180]')
  if( !is.lonlat(r))
    stop('Raster needs to be in a geographic coordinate system')
  ext.w <- ext(-180, west, -90,90)
  ext.e <- ext(east, 180, -90, 90)
  rw <- shift(crop(r, ext.w), dx=360)
  re <- shift(crop(r, ext.e), dx=-360)
  merge(merge(r,rw), re)
}

