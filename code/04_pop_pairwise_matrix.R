# populate pairwise data matrix

library(dplyr)

data <- readRDS("data/processed/env_data.rds")
env_pca <- readRDS("data/processed/env_pca.rds")
dist_matrix <- readRDS("data/processed/distance_matrix.rds")

pca <- env_pca$pca$x[,] %>% as.data.frame() %>% 
	dplyr::bind_cols(env_pca$env_data)

pairwise_matrices <- 1:length(data) %>%
	plyr::llply(function(j){
		sp_pca <- data[[j]] %>%
			dplyr::inner_join(pca)
		
		pca_dist <- 1:3 %>%
			lapply(function(i){
				d <- sp_pca[, paste0("PC", i)] %>%
					dist() %>% as.matrix() %>% as.data.frame.table()
				names(d)[3] <- paste0("PC", i)
				d[, 3, drop = F]
			}) %>%
			dplyr::bind_cols()
		
		dist_matrix[[j]] %>% as.data.frame.table() %>%
			dplyr::rename(sample1 = Var1, sample2 = Var2, geo = Freq) %>%
			dplyr::bind_cols(pca_dist)
	}) %>%
	`names<-`(names(data))

for (i in 1:length(data)){
	write.csv(pairwise_matrices[[i]], 
						file = paste0("data/processed/pairwise_tables/", 
													names(data)[i], 
													".csv"), 
						row.names = F)
}
