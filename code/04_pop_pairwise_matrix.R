# populate pairwise data matrix

library(dplyr)

env_pca <- readRDS("data/processed/env_pca.rds")
dist_matrix <- readRDS("data/processed/distance_matrix.rds")

pca_dist <- 1:3 %>%
	lapply(function(i){
		d <- dist(env_pca$x[, i]) %>% as.matrix() %>% as.data.frame.table()
		names(d)[3] <- paste0("PCA", i)
		d[, 3, drop = F]
	}) %>%
	dplyr::bind_cols()


pairwise_matrix <- dist_matrix %>% as.data.frame.table() %>%
	dplyr::rename(sample1 = Var1, sample2 = Var2, geo = Freq) %>%
	dplyr::bind_cols(pca_dist)

write.csv(pairwise_matrix, file = "data/processed/pairwise_matrix.csv", row.names = F)
