# plot of environmental PCA1

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

locations <- readRDS("data/processed/locations.rds")

# subset env_data
sub_env_data <- env_data %>%
	dplyr::filter(x >= floor(min(locations$x)-2), 
								x <= ceiling(max(locations$x)+2),
								y >= floor(min(locations$y)-2),
								y <= ceiling(max(locations$y)+2)) %>%
	dplyr::filter(sstmin > 0,
								chlorange > 0)

coord <- sub_env_data %>%
	dplyr::select(x, y)
	

pca_values <- sub_env_data %>%
	dplyr::select(calcite:sstrange) %>% 
	lapply(log) %>%
	as.data.frame() %>%
	prcomp(center = T, scale = T) %$%
	x %>% as.data.frame()

sub_env_data %<>%
	dplyr::bind_cols(pca_values)


sub_env_data %>%
	dplyr::select(x, y, PC2:PC3) %>%
	tidyr::gather("axis", "value", PC2:PC3) %>%
	ggplot(aes(x = x, y = y)) +
	geom_tile(aes(fill = value)) +
	scale_fill_gradient2(low = "#0571b0", 
											 mid = "#f7f7f7", 
											 high = "#ca0020") +
	facet_wrap(~axis) +
	scale_x_continuous(expand = c(0,0.1)) +
	scale_y_continuous(expand = c(0,0.1)) + 
	theme_minimal() +
	theme(panel.grid = element_blank(), 
				panel.background = element_rect(fill = "#bababa"), 
				legend.position = "none") +
	xlab("longitude") +
	ylab("latitude") + 
	coord_quickmap()


sub_env_data %>%
	dplyr::select(x, y, PC1) %>%
	# tidyr::gather("axis", "value", PC1:PC3) %>%
	ggplot(aes(x = x, y = y)) +
	geom_tile(aes(fill = PC1)) +
	scale_fill_gradient2(low = "#0571b0", 
											 mid = "#f7f7f7", 
											 high = "#ca0020") +
	geom_point(aes(x = x, y = y), data = locations, shape = 21) +
	# facet_wrap(~axis, ncol = 1) +
	scale_x_continuous(expand = c(0,0.1)) +
	scale_y_continuous(expand = c(0,0.1)) + 
	theme_minimal() +
	theme(panel.grid = element_blank(), 
				panel.background = element_rect(fill = "#bababa")) +
	xlab("longitude") +
	ylab("latitude") + 
	coord_quickmap()
