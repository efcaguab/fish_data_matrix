# get data from BioOracle for the GPS fixes in `locations.csv` outputs
# 'locations.rds' with details about the study sites and `env_data.rds` with the
# raw environmental data from BioOracle. Rows are in same order as in env_data.rds

library(magrittr)
library(ggplot2)
library(dplyr)
library(SDMTools)
library(stringr)
library(plyr)

# read data

folder <- "./data/raw/BioOracle_7070RV"
raster <- folder %>%
	list.files(full.names = T)
raster_names <- list.files(folder) %>%
	stringr::str_split("\\.") %>%
	lapply(`[`, 1) %>%
	unlist()
env_data <- SDMTools::asc2dataframe(raster, raster_names) %>% 
	dplyr::tbl_df()

# read locations
locations <- read.csv("data/raw/locations.csv")

# subset data
env_data %<>%
	dplyr::filter(round(y) %in% round(locations$latitude),
								round(x) %in% round(locations$longitude))

data <- locations %>%
	plyr::ddply("population", function(x){
		i <- geosphere::distHaversine(c(x$longitude, x$latitude), 
																	as.matrix(env_data[, c("x", "y")])) %>%
			which.min()
		env_data %>%
			dplyr::slice(i) %>%
			cbind(x, .)
	})

locations <- data %>%
	dplyr::select(name:x)

data %<>% 
	dplyr::select(calcite:sstrange)

saveRDS(locations, file = "data/processed/locations.rds", ascii = T, compress = F)
saveRDS(data, file = "data/processed/env_data.rds", ascii = T, compress = F)

