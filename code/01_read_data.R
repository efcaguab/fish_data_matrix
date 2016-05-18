# get data from BioOracle for the GPS fixes in `locations.csv` outputs
# 'locations.rds' with contains a list of the study sites per fish and `env_data.rds` with the
# raw environmental data from BioOracle per fish. Rows are in same order as in env_data.rds

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

# read species
species <- "./data/raw/species" %>%
	list.files(full.names = T) %>%
	lapply(read.csv)

nam <- "./data/raw/species" %>%
	list.files(full.names = F) %>%
	stringr::str_sub(end = -5)

names(species) <- nam
locations <- read.csv("data/raw/locations.csv")

env_data_filtered <- lapply(species, function(x){
	env_data %>%
		dplyr::filter(round(y) %in% round(x$latitude),
									round(x) %in% round(x$longitude))
})

for(i in 1:length(species)){
	env_data_filtered[[i]] <- species[[i]] %>%
		plyr::ddply("population", function(x){
			j <- geosphere::distHaversine(c(x$longitude, x$latitude), 
																		as.matrix(env_data_filtered[[i]][, c("x", "y")])) %>%
				which.min()
			env_data_filtered[[i]] %>%
				dplyr::slice(j) %>%
				cbind(x, .)
		})
}

locations <- env_data_filtered %>%
	lapply(dplyr::select, name:x)

data <- env_data_filtered %>% 
	lapply(dplyr::select, calcite:sstrange)

saveRDS(locations, file = "data/processed/locations.rds", ascii = T, compress = F)
saveRDS(data, file = "data/processed/env_data.rds", ascii = T, compress = F)

