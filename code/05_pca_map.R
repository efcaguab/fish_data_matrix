# plot of environmental PCA1

library(magrittr)
library(ggplot2)
library(dplyr)
library(SDMTools)
library(stringr)
library(plyr)
library(cowplot)
library(ggfortify)
library(ggrepel)

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

env_pca <- readRDS("data/processed/env_pca.rds")
locations <- env_pca$coord

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
	predict(env_pca$pca, newdata = .) %>%
	as.data.frame()

sub_env_data %<>%
	dplyr::bind_cols(pca_values)

sites <- env_pca$env_data %>% 
	dplyr::filter(name != c("Bay_de_Ghoubett"))

axis <- "PC1"

plot_axis <- function(axis, w, h, labels = F){
	p <- sub_env_data %>%
		dplyr::select_("x", "y", axis) %>%
		ggplot(aes(x = x, y = y)) +
		geom_tile(aes_string(fill = axis)) +
		scale_fill_gradient2(low = "#0571b0", 
												 mid = "#f7f7f7", 
												 high = "#ca0020",
												 name = "", 
												 na.value = "red") + 
		geom_point( data = sites,
								aes(x = longitude, y = latitude), 
								shape = 21)
	
	if(labels){
		p <- p +
			geom_text_repel(data = sites,
								aes(x = longitude, 
										y = latitude, 
										label = paste(1:length(name),
																	gsub("_", " ", name))), 
								size = 2)
	} else {
		p <- p +
			geom_text_repel(data = sites,
											aes(x = longitude, 
													y = latitude, 
													label = paste(1:length(name))), 
											size = 2)
	}
	
	p <- p +
		scale_x_continuous(expand = c(0,0.1), 
											 labels = function(x) paste0(x, "°E"), 
											 sec.axis = dup_axis(labels = NULL)) +
		scale_y_continuous(expand = c(0,0.1),
											 labels = function(x) paste0("  ", x, "°N"), 
											 position = "right", 
											 sec.axis = dup_axis(labels = NULL)) + 
		theme_bw() +
		theme(panel.grid = element_blank(), 
					panel.background = element_rect(fill = "grey90"), 
					axis.title = element_blank(), 
					panel.border = element_rect(colour = "black", fill = NA),
					axis.text.x = element_text(size = 5, margin = unit(rep(1,4), "lines")),
					axis.text.y = element_text(size = 5, 
																		 margin = unit(rep(2,4), "lines"),
																		 hjust = 0),
					legend.text = element_text(size = 5),
					legend.margin = margin(),
					legend.position = "right",
					# legend.key.width = unit(0.5, "lines"),
					# legend.key.height = unit(0.5, "lines"),
					axis.ticks = element_line(size = 0.3),
					axis.ticks.length=unit(-0.2, "lines")) +
		guides(fill = guide_colorbar(barwidth = unit(0.5, "lines"), 
																 barheight = unit(12, "lines"))) +
		xlab("longitude") +
		ylab("latitude") + 
		coord_quickmap()
	
p
	
	# ggsave(paste0(axis, ".pdf"), p, width = w, height = h)
}


# loading plots

point <- cbind(env_pca$env_data, env_pca$pca$x) %>% 
	dplyr::filter(name != c("Bay_de_Ghoubett"))

loading_plot_theme <- theme_bw() +
	theme(
				# panel.grid = element_blank(), 
				panel.background = element_rect(fill = "white"), 
				axis.title = element_text(size = 6, 
																	margin = unit(0, "lines")),
				panel.border = element_rect(colour = "black", fill = NA),
				axis.text.x = element_text(size = 5),
				axis.text.y = element_text(size = 5),
				legend.text = element_text(size = 5),
				legend.margin = margin(),
				legend.key.width = unit(0.5, "lines"),
				axis.ticks = element_line(size = 0.3)
				# axis.ticks.length=unit(-0.2, "lines")
				)

p4 <- autoplot(env_pca$pca, 
				 loadings = T, 
				 alpha = 0,
				 # label = T,
				 # label.label = env_pca$env_data$name,
				 # label.repel = T, 
				 loadings.colour = "grey80", 
				 loadings.size = 0.5, 
				 loadings.label = T,
				 loadings.label.size = 2,
				 loadings.label.repel = F,
				 loadings.label.colour = "grey30",
				 x = 1,
				 y = 2) + 
	geom_point(data = point,
						 aes(x = scales::rescale(PC1, to = c(-0.25, 0.25)),
						 		y = scales::rescale(PC2, to = c(-0.25, 0.25))),
						 size = 1,
						 shape = 21, 
						 colour = "black") +
	geom_text_repel(data = point,
									aes(x = scales::rescale(PC1, to = c(-0.25, 0.25)),
											y = scales::rescale(PC2, to = c(-0.25, 0.25)),
											label = gsub("_", " ", 1:length(name))),
									size = 2,
									colour = "black") +
	loading_plot_theme

# ggsave("loadings_PC1PC2.pdf", width = 3.5, height = 2.45)

p5 <- autoplot(env_pca$pca, 
				 loadings = T, 
				 alpha = 0,
				 # label = T,
				 # label.label = env_pca$env_data$name,
				 # label.repel = T, 
				 loadings.colour = "grey80", 
				 loadings.size = 0.5, 
				 loadings.label = T,
				 loadings.label.size = 2,
				 loadings.label.repel = F,
				 loadings.label.colour = "grey30",
				 x = 2,
				 y = 3) +
	geom_point(data = point,
						 aes(x = scales::rescale(PC2, to = c(-0.25, 0.25)),
						 		y = scales::rescale(PC3, to = c(-0.25, 0.25))),
						 size = 1,
						 shape = 21, 
						 colour = "black") +
	geom_text_repel(data = point,
									aes(x = scales::rescale(PC2, to = c(-0.25, 0.25)),
											y = scales::rescale(PC3, to = c(-0.25, 0.25)),
											label = gsub("_", " ", 1:length(name))),
									size = 2,
									colour = "black") +
	loading_plot_theme

# ggsave("loadings_PC2PC3.pdf", width = 3.5, height = 2.45)


p1 <- plot_axis("PC1", 7, 3.25, labels = T)
leg <- cowplot::get_legend(p1)
p2 <- plot_axis("PC2", 3.5, 2.45) + theme(legend.position = "none")
p3 <- plot_axis("PC3", 3.5, 2.45) + theme(legend.position = "none")

p2_p3 <- cowplot::plot_grid(p2, p3, 
														ncol = 1, 
														labels = c("(b)", "(c)"), 
														label_size = 8, 
														label_fontface = "plain", 
														label_y = 0.26, 
														label_x = 0.01)

p1_p2_p3 <- cowplot::plot_grid(p1 + theme(legend.position = "none"),
															 p2_p3, 
															 leg, 
															 ncol = 3, rel_widths = c(2.1, 1, 0.15), 
															 labels = c("(a)"), 
															 label_fontface = "plain", 
															 label_size = 8, 
															 label_y = 0.13, 
															 label_x = 0.01,
															 align = "hv")

p4_p5 <- cowplot::plot_grid(p4, p5, ncol = 2,
														labels = c("(d)", "(e)"), 
														label_fontface = "plain", 
														label_size = 8, 
														label_y = 0.25, 
														label_x = 0.13)

p <- cowplot::plot_grid(p1_p2_p3, p4_p5, ncol = 1, 
												rel_heights = c(1.7,1), 
												align = "hv")
cowplot::ggsave("fig3.pdf", p, width = 7, height = 6)
