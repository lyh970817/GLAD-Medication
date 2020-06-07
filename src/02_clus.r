setwd("../")
library(ProjectTemplate)
library(ggdendro)
library(ggrepel)
library(factoextra)
library(manipulate)
load.project()

# Original data

## Multi-dimensional scaling
mds <- mds_plot(dist(med_mat))

hc <- hclust(dist(med_mat))
hc_p <- hc_plot(hc)

cache("hc")
cache("hc_p")

## Hierarchical clustering single link hc <- hclust(dist(med_mat)) hc_p <- hc_plot(hc) Kmeans cluttering within-group sums of squares scree plot k <- k_scree(med_mat) cache("hc_p") cache("k") cache("mds") Cosine distance

cos_dist <- (med_mat / sqrt(rowSums(med_mat * med_mat))) %>%
  (function(X) {
    X %*% t(X)
  }) %>%
  (function(X) 1 - X) %>%
  as.dist()

cache("cos_dist")

## Hierarchical clustering single link
hc_cos <- hclust(cos_dist)
hc_cos_p <- hc_plot(hc_cos)

## Multi-dimensional scaling
mds_cos_fun <- function(n) {
  mds_plot(cos_dist, list(cutree(hc_cos, n)), type = "hc")
}

mds_cos_fun(4)

cache("hc_cos")
cache("hc_cos_p")
cache("mds_cos_fun")

# Normalize observations with the norm

# I sort of came up with this myself and realized this seemed to be called
# "L2 normalization" in machine learning terminology

norm_med_mat <- med_mat %>%
  apply(1, function(r) {
    r / sqrt(sum(r^2))
  }) %>%
  t()

cache("norm_med_mat")

## Hierarchical clustering single link
hc_norm <- hclust(dist(norm_med_mat))
hc_norm_p <- hc_plot(hc_norm)

# Squred Eucledian distance on normalized vectors are linearly connected to
# cosine distance
# https://stats.stackexchange.com/questions/299013/cosine-distance-as-similarity-measure-in-kmeans

k_norm <- k_scree(norm_med_mat)

## Multi-dimensional scaling

mds_norm_fun <- function(n) {
  mds_plot(dist(norm_med_mat),
    list(
      kmeans(norm_med_mat, center = n)$cluster,
      cutree(hc_norm, n)
    ),
    type = c("kmeans", "hc")
  )
}

mds_norm_fun(4)

cache("hc_norm")
cache("hc_norm_p")
cache("k_norm")
cache("mds_norm_fun")
ggsave("./graphs/mds_norm.png", mds_norm)

# Correlation based distance

cor_dist <- get_dist(med_mat, method = "pearson")
cache("cor_dist")

## Hierarchical clustering
hc_cor <- hclust(cor_dist)
hc_cor_p <- hc_plot(hc_cor)

## Multi-dimensional scaling
mds_cor_fun <- function(n) {
  mds_plot(cor_dist, list(cutree(hc_cor, n)), type = "hc")
}

mds_cor_fun(4)

cache("hc_cor")
cache("hc_cor_p")
cache("mds_cor_fun")

# Standardized data

# I think by standardizing the data we are saying that each attribute
# should contribute more or less the same to the distance, regardless of
# the size of the scale of it. But this is not what we want, because if a
# medication is low on every attribute, it wil be seen as being far from
# another medication that is high on every attribute even if the relative
# contribution of each attribute (the orientation of the medications) are
# similar.

# std_med_mat <- scale(med_mat)

# hc_5 <- hc_cluster(dist(std_med_mat))
# mds_5 <- mds_plot(dist(std_med_mat), kmeans(std_med_mat, center = 4)$cluster)
