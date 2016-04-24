# using the output of '01_read_data.R' this script explores the correlations
# between matrices and calculates the PCA

library(ggplot2)
library(tidyr)
library(corrplot)


# read data
data <- readRDS("data/processed/env_data.rds")

# log transform
log_data <- data %>%
	lapply(log) %>%
	as.data.frame()
# scale raw data to the same scale
scaled_data <- log_data %>%
	lapply(scale) %>%
	as.data.frame()

# have a look at the raw data
log_data %>%
	tidyr::gather("variable", "value") %>%
	ggplot(aes(x = value)) +
	geom_histogram(bins = 8) +
	facet_wrap(~ variable, scales = "free")

## correlation matrix including significances at the 0.05 level
# function for the test
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
# plot correlations
corrplot::corrplot(cor(scaled_data), p.mat = res1[[1]], 
									 sig.level=0.05, 
									 order = "hclust",
									 method = "number")

# pca
pca <- prcomp(log_data, center = T, scale = T)
summary(pca)
biplot(pca)

saveRDS(pca, file = "data/processed/env_pca.rds", ascii = T, compress = F)
