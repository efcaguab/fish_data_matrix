tsv <- read.delim("./data/raw/sites_gps.tsv")
pca <- readRDS("./data/processed/env_pca.rds")

env_data <- pca$env_data %>%
	dplyr::mutate_if(is.factor, funs(as.character)) %>%
	dplyr::mutate(name = gsub("_", " ", name)) %>%
	dplyr::bind_cols(as.data.frame(pca$pca$x))

tsv %<>%
	dplyr::mutate_if(is.factor, funs(as.character)) %>%
	dplyr::mutate(sitename = ifelse(sitename == "Jazirat Burqan", "Jazirat Baraqan", sitename)) %>%
	dplyr::filter(!sitename == "Hallinyats")

coordinates <- env_data %>%
	dplyr::select(name, PC1, PC2, PC3)

tsv %<>%	
	dplyr::select(-starts_with("pc")) %>%
	dplyr::left_join(coordinates, by = c("sitename" = "name"))

names(tsv) <- tolower(names(tsv))

write.table(tsv, "./data/processed/sites_pc-scores.tsv", sep = "\t", quote = F,
						row.names = F)
