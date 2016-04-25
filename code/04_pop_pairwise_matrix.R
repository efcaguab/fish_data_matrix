# populate pairwise data matrix

library(dplyr)

env_pca <- readRDS("data/processed/env_pca.rds")
dist_matrix <- readRDS("data/processed/distance_matrix.rds")

pairwise_matrix <- dist_matrix %>% as.data.frame.table() %>%
	dplyr::rename(sample1 = Var1, sample2 = Var2, geo = Freq)

write.csv(pairwise_matrix, file = "data/processed/pairwise_matrix.csv", row.names = F)
