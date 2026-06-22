#' ---
#' output:
#'   pdf_document: default
#'   html_document: default
#' ---
#' # Cheese Data Analysis
#' 
#' 
## ---------------------------------------------------------------------------------------------------------------
cheese_df <- read.csv("Cheese_Chemical_Data.csv")
head(cheese_df)


#' 
#' ### Data Validation
#' 
#' Checking for null values
#' 
## ---------------------------------------------------------------------------------------------------------------
cheese_df[!complete.cases(cheese_df),]


#' 
#' Summary Statistics for the chemical Variables:
#' 
## ---------------------------------------------------------------------------------------------------------------
summary(cheese_df[, colnames(cheese_df)[4:54]])

#' 
#' Checking variance for each column:
## ---------------------------------------------------------------------------------------------------------------
X <- cheese_df[, colnames(cheese_df)[4:54]]
log_X <- log(X)
vars <- sapply(X, var, na.rm = T)
vars 

vars_log <- sapply(log_X, var, na.rm = T)
vars_log 

#' 
#' Visually analyzing both raw and log-transformed distributions of chemicals:
#' 
## ---------------------------------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(tidyverse)
chem_cols <- colnames(cheese_df)[4:54]

raw_long <- cheese_df |>
  select(PrimaryID, all_of(chem_cols)) |>
  pivot_longer(
    cols = all_of(chem_cols),
    names_to = "chemical",
    values_to = "value"
  ) |>
  mutate(scale = "Raw")

log_long <- cheese_df |>
  select(PrimaryID, all_of(chem_cols)) |>
  mutate(across(all_of(chem_cols), log)) |>
  pivot_longer(
    cols = all_of(chem_cols),
    names_to = "chemical",
    values_to = "value"
  ) |>
  mutate(scale = "Log-transformed")

plot_long <- bind_rows(raw_long, log_long)
rbind(head(plot_long), tail(plot_long))

#' 
#' 
## ---------------------------------------------------------------------------------------------------------------
library(ggplot2)
library(tidyverse)

plot_hist_density_long <- function(long_df, cols, bins = 20, ncol = 2) {
  
  plot_df <- long_df %>%
    filter(chemical %in% cols)
  
  if (nrow(plot_df) == 0) {
    stop("None of the supplied columns were found in long_df$chemical.")
  }
  
  ggplot(plot_df, aes(x = value)) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = bins,
                   fill = "skyblue",
                   color = "white",
                   alpha = 0.7) +
    geom_density(color = "red", linewidth = 0.8, na.rm = TRUE) +
    facet_wrap(~ chemical, scales = "free", ncol = ncol) +
    theme_minimal() +
    labs(
      title = "Histograms with Density Curves",
      x = "Value",
      y = "Density"
    ) +
    theme(
      strip.text = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}


#' 
#' 
#' Features 1-10:
#' 
#' Raw Scale:
## ---------------------------------------------------------------------------------------------------------------
plot_hist_density_long(raw_long, chem_cols[1:10])

#' 
## ---------------------------------------------------------------------------------------------------------------
plot_hist_density_long(log_long, chem_cols[1:10])

#' 
#' Features 11-20:
#' 
#' Raw Scale:
## ---------------------------------------------------------------------------------------------------------------
plot_hist_density_long(raw_long, chem_cols[11:20])

#' 
## ---------------------------------------------------------------------------------------------------------------
plot_hist_density_long(log_long, chem_cols[11:20])

#' 
#' 
#' Features 21-30:
#' 
#' Raw Scale:
## ---------------------------------------------------------------------------------------------------------------
plot_hist_density_long(raw_long, chem_cols[21:30])

#' 
## ---------------------------------------------------------------------------------------------------------------
plot_hist_density_long(log_long, chem_cols[21:30])

#' 
#' 
#' Features 31-40:
#' 
#' Raw Scale:
## ---------------------------------------------------------------------------------------------------------------
plot_hist_density_long(raw_long, chem_cols[31:40])

#' 
## ---------------------------------------------------------------------------------------------------------------
plot_hist_density_long(log_long, chem_cols[31:40])

#' 
#' 
#' Features 41-51:
#' 
#' Raw Scale:
## ---------------------------------------------------------------------------------------------------------------
plot_hist_density_long(raw_long, chem_cols[41:51])

#' 
## ---------------------------------------------------------------------------------------------------------------
plot_hist_density_long(log_long, chem_cols[41:51])

#' 
#' 
#' It seems reasonable to use the log-transformed variables, instead of the raw scale.
#' 
#' PCA on log-transformed chemical data:
#' 
## ---------------------------------------------------------------------------------------------------------------
pca_fit <- prcomp(log_X, center = TRUE, scale. = TRUE)

# Variance explained
eig <- pca_fit$sdev^2
var_explained <- eig / sum(eig)
cum_var_explained <- cumsum(var_explained)

pca_var_tbl <- tibble(
  PC = paste0("PC", seq_along(eig)),
  eigenvalue = eig,
  prop_var = var_explained,
  cum_prop_var = cum_var_explained
)

pca_var_tbl



#' 
#' Scree plot:
## ---------------------------------------------------------------------------------------------------------------
ggplot(pca_var_tbl, aes(x = seq_along(PC), y = prop_var)) +
  geom_point(size = 2) +
  geom_line() +
  scale_x_continuous(breaks = seq_len(nrow(pca_var_tbl))) +
  labs(
    x = "Principal Component",
    y = "Proportion of Variance Explained",
    title = "Scree Plot"
  ) +
  theme_minimal()



#' 
#' Let's consider the first 8 PCs, based on the scree plot.
#' 
## ---------------------------------------------------------------------------------------------------------------
scores_df <- as_tibble(pca_fit$x) %>%
  mutate(PrimaryID = cheese_df$PrimaryID) %>%
  relocate(PrimaryID)

scores_df %>% dplyr::select(PrimaryID, PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8)



#' 
#' Plotting the first 2 PCs:
#' 
## ---------------------------------------------------------------------------------------------------------------
ggplot(scores_df, aes(x = PC1, y = PC2, label = PrimaryID)) +
  geom_point(size = 3) +
  geom_text(nudge_y = 0.2, size = 3) +
  labs(
    title = "Cheese Scores on PC1 and PC2",
    x = paste0("PC1 (", round(100 * pca_var_tbl$prop_var[1], 1), "%)"),
    y = paste0("PC2 (", round(100 * pca_var_tbl$prop_var[2], 1), "%)")
  ) +
  theme_minimal()



#' 
#' 
#' It looks like the first 2 PCs do a good job of seperating different cheese samples and are 
#' uncovering some patterns/trends.
#' 
#' Biplot:
#' 
## ---------------------------------------------------------------------------------------------------------------
biplot(pca_fit, scale = 0, cex = 0.7)


#' 
#' Interpretation: Use the biplot to find similar chemicals, long arrows mean they are important to the 
#' PC under study, same direction means positive relationship between chemicals, orthogonal arrows
#' mean they have weak relationship.
#' 
#' We will extract the loadings to see which cheese's drive which PC:
## ---------------------------------------------------------------------------------------------------------------
loadings_df <- as_tibble(pca_fit$rotation, rownames = "PrimaryID")

loadings_df %>% dplyr::select(PrimaryID, PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8)



#' 
#' Based on the magnitude of the loadings we decide how strongly a chemical influences a PC and based 
#' on the sign we decide the direction of this relationship.
#' 
#' Function to find top n loadings:
## ---------------------------------------------------------------------------------------------------------------
get_top_loadings <- function(loadings_df, pc_name, top_n = 10) {
  loadings_df %>%
    transmute(
      PrimaryID,
      loading = .data[[pc_name]],
      abs_loading = abs(.data[[pc_name]])
    ) %>%
    arrange(desc(abs_loading)) %>%
    slice_head(n = top_n)
}


#' 
#' Top 10 chemicals for PC1:
## ---------------------------------------------------------------------------------------------------------------
top_pc1 <- get_top_loadings(loadings_df, "PC1")
top_pc1

#' 
#' Top 10 chemicals for PC2:
## ---------------------------------------------------------------------------------------------------------------
top_pc2 <- get_top_loadings(loadings_df, "PC2")
top_pc2

#' 
#' Top 10 chemicals for PC3:
## ---------------------------------------------------------------------------------------------------------------
top_pc3 <- get_top_loadings(loadings_df, "PC3")
top_pc3

#' 
#' 
#' Top 10 chemicals for PC4:
## ---------------------------------------------------------------------------------------------------------------
top_pc4 <- get_top_loadings(loadings_df, "PC4")
top_pc4

#' 
#' 
#' Top 10 chemicals for PC5:
## ---------------------------------------------------------------------------------------------------------------
get_top_loadings(loadings_df, "PC5")

#' 
#' 
#' Top 10 chemicals for PC6:
## ---------------------------------------------------------------------------------------------------------------
get_top_loadings(loadings_df, "PC6")

#' 
#' 
#' Top 10 chemicals for PC7:
## ---------------------------------------------------------------------------------------------------------------
get_top_loadings(loadings_df, "PC7")

#' 
#' 
#' Top 10 chemicals for PC8:
## ---------------------------------------------------------------------------------------------------------------
get_top_loadings(loadings_df, "PC8")

#' 
#' No single chemical loads very strongly on any PC, there's a lot of cross loading and shared variance.
#' 
## ---------------------------------------------------------------------------------------------------------------
pc_summary <- list(
  PC1 = top_pc1,
  PC2 = top_pc2,
  PC3 = top_pc3,
  PC4 = top_pc4
)

pc_summary

#' 
#' The first 4 PCs explain 65% of the variability and seem reasonable, we will retain those, but we 
#' still have upto 8 PCs we might use them to uncover some patterns after clustering.
#' 
#' To validate the PCA and check if the patterns (chemicals that load strongly on the first 4 PCs)
#' actually have some pattern of going together we will cluster the chemicals to find chemicals that 
#' are similar or close to each other based on a correlation-based distance metric, so our vector will 
#' represent a chemical and the elements of the vector represent the different cheeses.
#' 
#' Heirarchical Clustering:
#' 
## ---------------------------------------------------------------------------------------------------------------
log_X_scaled <- scale(log_X, center = T, scale = T)
chem_mat <- t(log_X_scaled)
dim(chem_mat)

#' 
#' For distance metric we will use 1-r, this computes similarity between chemicals based on how they 
#' vary across cheeses,
#' 
## ---------------------------------------------------------------------------------------------------------------
chem_cor <- cor(t(chem_mat), use = "pairwise.complete.obs")
chem_dist <- as.dist(1 - abs(chem_cor))

#' 
#' This metric does not care about the direction only the strength of the relationship.
#' 
#' Dendogram:
#' 
## ---------------------------------------------------------------------------------------------------------------
hc_chem <- hclust(chem_dist, method = "average")

plot(hc_chem, main = "Hierarchical Clustering of Chemicals",
     xlab = "", sub = "", cex = 0.8)


#' 
#' I think having 6 clusters seem reasonable.
#' 
## ---------------------------------------------------------------------------------------------------------------
k <- 4
chem_clusters <- cutree(hc_chem, k = k)

cluster_df <- tibble(
  chemical = names(chem_clusters),
  cluster = chem_clusters
) %>%
  arrange(cluster)

#cluster_df
cluster_df %>% group_by(as.factor(cluster)) %>% summarise(n = n())

#' 
#' 
#' Let's look at a heatmap with the clusters:
## ---------------------------------------------------------------------------------------------------------------
ordered_chemicals <- hc_chem$labels[hc_chem$order]

heatmap_mat <- chem_mat[ordered_chemicals, ,drop = FALSE]

heatmap(heatmap_mat,
        Rowv = as.dendrogram(hc_chem),
        Colv = NA,
        scale = "none",
        margins = c(6, 10),
        col = colorRampPalette(c("navy", "white", "firebrick3"))(100))



#' 
#' Validating results, does 4 clusters make sense:
## ---------------------------------------------------------------------------------------------------------------
library(cluster)
k <- 5
cluster_assign <- cutree(hc_chem, k = k)

sil <- silhouette(cluster_assign, chem_dist)

summary(sil)

#' 
## ---------------------------------------------------------------------------------------------------------------
mean_sil <- mean(sil[, "sil_width"])
mean_sil

#' 
#' silhouette plot:
## ---------------------------------------------------------------------------------------------------------------
plot(sil, border = NA, main = "Silhouette Plot for k = 4")

#' We would like avg Silhouette widths to be > 0.25 for each cluster
#' 
## ---------------------------------------------------------------------------------------------------------------
sil_df <- as.data.frame(sil[, 1:3]) %>%
  rownames_to_column("chemical") %>%
  as_tibble() %>%
  rename(
    cluster = cluster,
    neighbor = neighbor,
    sil_width = sil_width
  ) %>%
  arrange(cluster, desc(sil_width))

sil_df[sil_df$sil_width<0.2,]


#' 
#' Cluster-level averages:
#' 
## ---------------------------------------------------------------------------------------------------------------
sil_cluster_summary <- sil_df %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    avg_sil_width = mean(sil_width),
    min_sil_width = min(sil_width),
    n_negative = sum(sil_width < 0)
  )

sil_cluster_summary


#' 
#' Cluster 3 is kind of alarming when k = 5.
#' 
#' Finding optimal number of clusters:
#' 
## ---------------------------------------------------------------------------------------------------------------
compare_k <- function(hc_obj, dist_obj, k_values = 2:8) {
  map_dfr(k_values, function(k) {
    cl <- cutree(hc_obj, k = k)
    sil <- silhouette(cl, dist_obj)
    
    tibble(
      k = k,
      avg_silhouette = mean(sil[, "sil_width"]),
      min_cluster_size = min(table(cl)),
      max_cluster_size = max(table(cl)),
      n_negative = sum(sil[, "sil_width"] < 0)
    )
  })
}

k_summary <- compare_k(hc_chem, chem_dist, k_values = 2:8)
k_summary


#' 
#' None of these k's do a very good job of clustering, so we'll use it as an exploratory tool. I will
#' go ahead with k = 4, it seems reasonable and the tradeoff is fair. 
#' 
#' I am going forward with 4 clusters since that avoids clusters with too few features, the next step
#' is to compare whether they load on the same PC, i.e. whether features in the same cluster load 
#' strongly to the same PC.
#' 
#' 
#' Comparing the 4 clusters with the 4 PCs:
## ---------------------------------------------------------------------------------------------------------------

k <- 4

cluster_assign <- cutree(hc_chem, k = k)

loadings_df <- as.data.frame(pca_fit$rotation[, 1:4]) %>%
  rownames_to_column("chemical") %>%
  as_tibble()

# cluster table
cluster_df <- tibble(
  chemical = names(cluster_assign),
  cluster = as.integer(cluster_assign)
)

# merge and compute dominant PC info
compare_df <- cluster_df %>%
  left_join(loadings_df, by = "chemical") %>%
  rowwise() %>%
  mutate(
    dominant_PC = c("PC1", "PC2", "PC3", "PC4")[which.max(abs(c(PC1, PC2, PC3, PC4)))],
    dominant_loading = c(PC1, PC2, PC3, PC4)[which.max(abs(c(PC1, PC2, PC3, PC4)))],
    loading_sign = ifelse(dominant_loading >= 0, "Positive", "Negative"),
    max_abs_loading = max(abs(c(PC1, PC2, PC3, PC4)))
  ) %>%
  ungroup() %>%
  arrange(cluster, dominant_PC, desc(max_abs_loading))

compare_df



#' 
#' Checking whether each cluster is mostly associated with 1 PC:
## ---------------------------------------------------------------------------------------------------------------
cluster_pc_summary <- compare_df %>%
  count(cluster, dominant_PC, loading_sign) %>%
  group_by(cluster) %>%
  mutate(prop = n / sum(n)) %>%
  arrange(cluster, desc(prop))

cluster_pc_summary


#' 
#' Average loading patterns by cluster:
## ---------------------------------------------------------------------------------------------------------------
cluster_loading_summary <- compare_df %>%
  group_by(cluster) %>%
  summarise(
    n_features = n(),
    mean_abs_PC1 = mean(abs(PC1)),
    mean_abs_PC2 = mean(abs(PC2)),
    mean_abs_PC3 = mean(abs(PC3)),
    mean_abs_PC4 = mean(abs(PC4))
  )

cluster_loading_summary



#' 
#' Whether chemicals within same cluster have similar PCA loading patterns:
## ---------------------------------------------------------------------------------------------------------------
plot_df <- compare_df %>%
  select(chemical, cluster, PC1, PC2, PC3, PC4) %>%
  pivot_longer(
    cols = starts_with("PC"),
    names_to = "PC",
    values_to = "loading"
  ) %>%
  mutate(
    chemical = factor(
      chemical,
      levels = compare_df %>% arrange(cluster) %>% pull(chemical)
    )
  )

ggplot(plot_df, aes(x = PC, y = chemical, fill = loading)) +
  geom_tile(color = "white") +
  facet_wrap(~ cluster, scales = "free_y", ncol = 2) +
  scale_fill_gradient2(
    low = "navy",
    mid = "white",
    high = "firebrick3",
    midpoint = 0
  ) +
  theme_minimal() +
  labs(
    title = "PCA Loadings by Chemical Cluster",
    x = "Principal Component",
    y = "Chemical"
  )

#' 
#' We don't have a very good structure from the PCA and clustering but it's acceptable, we will use these
#' later on to validate the results of our analysis, we will save the results from our analysis and
#' move to screening the most important chemicals.
#' 
## ---------------------------------------------------------------------------------------------------------------
k_clusters <- 4
cluster_assign <- cutree(hc_chem, k = k_clusters)

chemical_annotation <- as.data.frame(pca_fit$rotation[, 1:4]) %>%
  rownames_to_column("chemical") %>%
  as_tibble() %>%
  rename(
    PC1_loading = PC1,
    PC2_loading = PC2,
    PC3_loading = PC3,
    PC4_loading = PC4
  ) %>%
  rowwise() %>%
  mutate(
    cluster_4 = unname(cluster_assign[chemical]),
    dominant_PC = c("PC1", "PC2", "PC3", "PC4")[which.max(abs(c(PC1_loading, PC2_loading, PC3_loading, PC4_loading)))],
    dominant_loading = c(PC1_loading, PC2_loading, PC3_loading, PC4_loading)[which.max(abs(c(PC1_loading, PC2_loading, PC3_loading, PC4_loading)))],
    loading_sign = ifelse(dominant_loading >= 0, "Positive", "Negative"),
    max_abs_loading = max(abs(c(PC1_loading, PC2_loading, PC3_loading, PC4_loading)))
  ) %>%
  ungroup() %>%
  arrange(cluster_4, dominant_PC, desc(max_abs_loading))

write.csv(chemical_annotation, "chemical_data_reduced_pca_cluster_k4.csv", row.names = FALSE)

#' 
#' 
## ---------------------------------------------------------------------------------------------------------------
write.csv(scores_df, "PCA_Scores_by_Cheese.csv", row.names = F)

write.csv(loadings_df, "PCA_Loadings.csv", row.names = F)

#' 
#' Now we will move to the screening step.
