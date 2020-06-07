hc_plot <- function(hc) {
  ggdendrogram(hc, rotate = TRUE, theme_dendro = FALSE) +
    theme(
      panel.grid.major.x = element_line(
        size = 0.5,
        linetype = "dashed",
        colour = "gray"
      ),
      axis.title.y = element_blank(),
      axis.text.x = element_text(colour = "black", size = 12),
      axis.text.y = element_text(colour = "black", size = 12),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      panel.background = element_blank(),
      legend.position = "bottom"
    ) +
    labs(title = "Hierarchical Clustering of Medications", y = "", size = 15)
}

k_scree <- function(mat) {
  wss <- (nrow(mat) - 1) * sum(apply(mat, 2, var))
  for (i in 2:(nrow(mat) - 1)) {
    wss[i] <- sum(kmeans(mat,
      centers = i
    )$withinss)
  }

  k_scree_plot <- qplot(seq_along(wss), wss) +
    geom_point() +
    geom_line() +
    theme(
      panel.grid.major.x = element_line(
        size = 0.5,
        linetype = "dashed",
        colour = "gray"
      ),
      axis.title.y = element_blank(),
      axis.text.x = element_text(colour = "black", size = 12),
      axis.text.y = element_text(colour = "black", size = 12),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      panel.background = element_blank(),
      legend.position = "bottom"
    ) +
    scale_x_continuous(breaks = seq_along(wss)) +
    labs(
      title = "K-Means Within-group Sums of Squares Scree Plot",
      y = "Within groups sum of squares", x = "Number of Clusters",
      size = 15
    )
  return(k_scree_plot)
}

mds_plot <- function(dist_mat, cluster = NULL, type = NULL) {
  mds <- cmdscale(dist_mat, eig = TRUE, k = 2)

  mds_plot_dat <- tibble(
    med = dimnames(mds$points)[[1]],
    x = mds$points[, 1],
    y = mds$points[, 2]
  )

  if (!is.null(type)) {
    for (i in seq_along(type)) {
      mds_plot_dat[toupper(type[i])] <- as.factor(cluster[[i]])
    }
  }

  mds_plot <- ggplot(mds_plot_dat, aes(x = x, y = y, label = med)) +
    geom_text_repel() +
    theme(
      panel.grid.major.x = element_line(
        size = 0.5,
        linetype = "dashed",
        colour = "gray"
      ),
      axis.title.y = element_blank(),
      axis.text.x = element_text(colour = "black", size = 12),
      axis.text.y = element_text(colour = "black", size = 12),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      panel.background = element_blank(),
      legend.position = "bottom"
    ) +
    labs(
      subtitle = "Note that the clustersing is based on the data matrix (not the MDS matrix computed from the distance matrix)",
      y = "", x = "", color = "Hierarchical Clustering", shape = "K-means", size = 15
    )

  if (is.null(type)) {
    mds_plot <- mds_plot +
      ggtitle("Multidimensional Scaling Plot") +
      geom_point()
  } else if (all(type == "kmeans")) {
    mds_plot <- mds_plot +
      geom_point(aes(shape = KMEANS), size = 5) +
      ggtitle("K-Means Result on Multidimensional Scaling Plot") %>%
      scale_shape_manual(values = LETTERS)
  } else if (all(type == "hc")) {
    mds_plot <- mds_plot +
      geom_point(aes(color = HC), size = 5) +
      ggtitle("Hierarchical Clustering Result on Multidimensional Scaling Plot") +
      scale_shape_manual(values = LETTERS)
  } else if (all(c("hc", "kmeans") %in% type)) {
    mds_plot <- mds_plot +
      geom_point(aes(shape = KMEANS, color = HC), size = 5) +
      ggtitle("K-Means and Hierarchical Clustering Result on Multidimensional Scaling Plot") +
      scale_shape_manual(values = LETTERS)
  }

  return(mds_plot)
}
