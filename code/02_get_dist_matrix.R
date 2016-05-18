# read the data in 'locations.csv' and calculates a distance matrix between GPS 
# fixes output is 'dist_matrix.rds' which is populated with a 'cost' which is 
# directly proportional to length of the shortest path that doesn't runs on land
# between two points to illustrate the results there is one plot that shows the
# distance from point #1 to all possible locations in the grid

library(raster)
library(SDMTools)
library(gdistance)

# read locations
locations <- readRDS("data/processed/locations.rds")

# load and set up the raster
ras <- SDMTools::read.asc("data/raw/BioOracle_7070RV/calcite.asc") %>%
	SDMTools::raster.from.asc() %>%
	raster::`crs<-`("+proj=longlat +datum=WGS84")

# frame the thingy
limits <- locations %>%
	lapply(function(x) c(floor(min(x$x)-2),
											 ceiling(max(x$x)+2),
											 floor(min(x$y)-2),
											 ceiling(max(x$y)+2)))

limits <- do.call(rbind, limits)
limits <- c(min(limits[,1])-2, 
						max(limits[,2])+2, 
						min(limits[,3])-2,
						max(limits[,4])+2)

# crop raster
ras %<>% 
	raster::crop(raster::extent(limits))
land_mask <- ras

ras[] <- !is.na(ras[])
ras[] <- plyr::mapvalues(ras[], c(T, F), c(1, 9999))

# calculate transition matrix
tran <- ras %>%
	gdistance::transition(function(x) 1/mean(x), 8) %>%
	gdistance::geoCorrection(type = "c")

# get distance from the first point for ilustration
d <- gdistance::accCost(tran, as.matrix(locations[[1]][c(1), c("x", "y")])) %>%
	mask(land_mask)
d[] <- replace(d[], d[] > 6e6, NA)
plot(d)

# get distance matrices
dist_matrix <- lapply(locations, function(x) costDistance(tran, as.matrix(x[, c("x", "y")]))) %>%
	lapply(as.matrix)
	
saveRDS(dist_matrix, file = "data/processed/distance_matrix.rds", compress =  F, ascii = T)
