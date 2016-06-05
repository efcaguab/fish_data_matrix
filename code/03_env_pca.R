# using the output of '01_read_data.R' this script explores the correlations
# between matrices and calculates the PCA

library(ggplot2)
library(tidyr)
library(corrplot)


# read data
data <- readRDS("data/processed/env_data.rds")
locations <- readRDS("data/processed/locations.rds")

# log transform
log_data <- data %>%
	lapply(function(x){
		x %>%
			lapply(log) %>%
			as.data.frame()
	})
	
# full environmental data

env <- list()
for (i in 1:length(data)){
	env[[i]] <- cbind(locations[[i]] ,data[[i]])
}

env %<>% plyr::ldply()

sub_env_data <- env[!duplicated(env), ]

# pca
coord <- sub_env_data %>%
	dplyr::select(x, y)

pca <- sub_env_data %>%
	dplyr::select(calcite:sstrange) %>% 
	lapply(log) %>%
	as.data.frame() %>%
	prcomp(center = T, scale = T)

saveRDS(list(pca  = pca, coord = coord, env_data = sub_env_data), file = "data/processed/env_pca.rds", ascii = T, compress = F)

