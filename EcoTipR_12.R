#Install and call packages
install.packages("colorspace")
install.packages("dendextend")
library(colorspace)
library(dendextend)


# Correlation -------------------------------------------------------------
iris <- datasets::iris
iris <- iris[c(1:20, 51:70, 101:120), ]
iris2 <- iris[,-5]

species_labels <- iris[,5]
species_col <- rev(rainbow_hcl(3))[as.numeric(species_labels)]

# Plot a SPLOM:
par(bg = "grey95")
pairs(iris2, bg = species_col,
      lower.panel = NULL,
      cex.labels = 1.75, pch = 21, cex = 2, las = 2)

# Add a legend
par(xpd = TRUE)
legend(x = 0.01, y = 0.25, cex = 1.25,
       legend = rev(as.character(levels(species_labels))),
       fill   = rev(unique(species_col)), bty = "n")
par(xpd = NA)



# Cluster -----------------------------------------------------------------
d_iris <- dist(iris2)
hc_iris <- hclust(d_iris, method = "complete")
iris_species <- rev(levels(iris[,5]))

dend <- as.dendrogram(hc_iris)

# order it the closest we can to the order of the observations:
dend <- rotate(dend, 1:nrow(iris))

# Color the branches based on the clusters:
dend <- color_branches(dend, k=3) #, groupLabels=iris_species)

# Manually match the labels, as much as possible, to the real classification of the flowers:
labels_colors(dend) <- rainbow_hcl(3)[sort_levels_values(as.numeric(iris[,5])[order.dendrogram(dend)])]

# We shall add the flower type to the labels:
labels(dend) <- paste(as.character(iris[,5])[order.dendrogram(dend)],"(",labels(dend),")", sep = "")

# We hang the dendrogram a bit:
dend <- hang.dendrogram(dend,hang_height=0.25)

# reduce the size of the labels:
dend <- set(dend, "labels_cex", 0.75)

# And plot:
par(mar = c(0,1,2,4))
plot(dend, main = "Clustered Iris data set
     (the labels give the true flower species)", 
     horiz =  T,  nodePar = list(cex = .1), axes = FALSE)
legend("topleft", legend = iris_species, fill = rainbow_hcl(3), bty = "n")