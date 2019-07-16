# Use the sample shapefile_to_weighted_kernel_density.zip data files
# with this script. Make sure to uncompress them into your R session folder
library(spatstat)
library(maptools)
library(sf)
library(raster)
library(rgdal)

# Load shapefiles
w.sf <- st_read("Maine.shp")
p.sf <- st_read("points.shp")

# Convert to spatstat formats
p.sp  <- as(p.sf, "Spatial")  # Create Spatial* object
p.ppp <- as(p.sp, "ppp")      # Create ppp object

win <- as(w.sf, "Spatial")
win.ppp <- as(win, "owin")

# Define study extent (i.e. boundaries)
p.ppp$window <- win.ppp

# Create kernel density using the CID column as weight
# sigma = kernel search radius
# eps = output pixel size
d <- density(p.ppp, sigma = 10000, weights = marks(p.ppp)$CID, eps=1000)

# Export to raster
writeRaster(raster(d), "density.tiff", format="GTiff")
