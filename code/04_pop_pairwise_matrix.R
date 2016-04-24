# populate pairwise data matrix

library(dplyr)

env_pca <- readRDS("data/processed/env_pca.rds")
dist_matrix <- readRDS("data/processed/distance_matrix.rds")

dist_matrix %>% as.data.frame.table() %>%
	dplyr::rename(sample1 = Var1, sample2 = Var2, geo = Freq)
