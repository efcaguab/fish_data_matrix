library(raster)

# read locations
locations <- readRDS("data/processed/locations.rds")

# load and set up the raster
ras <- SDMTools::read.asc("data/raw/BioOracle_7070RV/calcite.asc") %>%
	SDMTools::raster.from.asc() %>%
	raster::`crs<-`("+proj=longlat +datum=WGS84")

ras %<>% 
	raster::crop(raster::extent(c(floor(min(locations$x)-2),
																ceiling(max(locations$x)+2),
																floor(min(locations$y)-2),
																ceiling(max(locations$y)+2))))
land_mask <- ras

ras[] <- !is.na(ras[])
ras[] <- plyr::mapvalues(ras[], c(T, F), c(1, 9999))

# calculate transition matrix
tran <- ras %>%
	gdistance::transition(function(x) 1/mean(x), 8) %>%
	gdistance::geoCorrection(type = "c")

# get distance from the first point for ilustration
d <- gdistance::accCost(tran, as.matrix(locations[c(1), c("x", "y")])) %>%
	mask(land_mask)
d[] <- replace(d[], d[] > 6e6, NA)
plot(d)

# get distance matrix
dist_matrix <- costDistance(tran, as.matrix(locations[, c("x", "y")]))
	
saveRDS(dist_matrix, file = "data/processed/distance_matrix.rds", compress =  F)
