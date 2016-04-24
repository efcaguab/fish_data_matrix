library(magrittr)
library(ggplot2)

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


# log transform
log_data <- data %>%
	lapply(log) %>%
	as.data.frame()
# scale

scaled_data <- log_data %>%
	lapply(scale) %>%
	as.data.frame()

# have a look at the raw data
log_data %>%
	tidyr::gather("variable", "value") %>%
	ggplot(aes(x = value)) +
	geom_histogram(bins = 8) +
	facet_wrap(~ variable, scales = "free")

# correlation matrix

cor.mtest <- function(mat, conf.level = 0.95){
	mat <- as.matrix(mat)
	n <- ncol(mat)
	p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
	diag(p.mat) <- 0
	diag(lowCI.mat) <- diag(uppCI.mat) <- 1
	for(i in 1:(n-1)){
		for(j in (i+1):n){
			tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
			p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
			lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
			uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
		}
	}
	return(list(p.mat, lowCI.mat, uppCI.mat))
}

res1 <- cor.mtest(scaled_data,0.95)
corrplot::corrplot(cor(scaled_data), p.mat = res1[[1]], 
									 sig.level=0.05, 
									 order = "hclust",
									 method = "number")

# pca
pca <- prcomp(log_data, center = T, scale = T)
summary(pca)
biplot(pca)
