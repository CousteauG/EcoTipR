# Install package ---------------------------------------------------------
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("ggpubr")) install.packages("ggpubr")
if(!require("colorspace")) install.packages("colorspace")
if(!require("wesanderson")) install.packages("wesanderson")
if(!require("ggrepel")) install.packages("ggrepel")
if(!require("patchwork")) install.packages("patchwork")
if(!require("factoextra")) install.packages("factoextra")
if(!require("FactoMineR")) install.packages("FactoMineR")
if(!require("rstanarm")) install.packages("rstanarm")
if(!require("bayestestR")) install.packages("bayestestR")
if(!require("insight")) install.packages("insight")
library(ggplot2)
library(patchwork)
library(wesanderson)
library(colorspace)
library(ggpubr)
library(ggrepel)
library(factoextra)
library(FactoMineR)
library(rstanarm)
library(bayestestR)
library(insight)


# LINEAR REGRESSION -------------------------------------------------------

# Load data
data(mtcars)

# Inspect the data
head(mtcars)

# Convert cyl as a grouping variable
mtcars$cyl <- as.factor(mtcars$cyl)

#REGRESSION PLOT
a <- ggscatter(mtcars, x = "wt", y = "mpg",
               add = "reg.line",                         # Add regression line
               conf.int = TRUE,                          # Add confidence interval
               color = "cyl", palette = "jco",           # Color by groups "cyl"
               shape = "cyl",                            # Change point shape by groups "cyl"
               title = "Regresión lineal") +                          
  stat_cor(aes(color = cyl, 
               label = paste(..rr.label.., cut(..p.., 
                                               breaks = c(-Inf, 0.0001, 0.001, 0.01, 0.05, Inf),
                                               labels = c("'****'", "'***'", "'**'", "'*'", "'ns'")), 
                             sep = "~")), digits = 3, label.x = 4, size = 4) +
  theme(axis.text.y   = element_text(size=13),
        axis.text.x   = element_text(size=13),
        axis.title.y  = element_text(size=13),
        axis.title.x  = element_text(size=13),
        panel.border  = element_rect(colour = "black", fill = NA, size=2),
        legend.title  = element_text(colour ="blue", size = 13, face="bold"),
        legend.text   = element_text(colour="blue", size = 13, face="bold")) +
  labs(y = "Miles/(US) gallon", x = "Weight (1000 lbs)")



# Bayesian Linear Regression ----------------------------------------------
# Load data
data("mtcars")

# Inspect the data
head(mtcars)


#Bayesian linear regression via STAN
model <- stan_glm(mpg ~ wt, data=mtcars)


# Extracting the posterior
posteriors <- insight::get_parameters(model)
head(posteriors)


#Plot posterior
b <- ggplot(posteriors, aes(x = wt)) +
  geom_density(fill = "orange")+
  ggtitle("Distribucion posteriori [Slope] (Bayesiano)") +
  labs(y = "Density", x = "Weight (1000 lbs)") +
  theme(axis.text.y     = element_text(size=13),
        axis.text.x     = element_text(size=13),
        axis.title.y    = element_text(size=13),
        axis.title.x    = element_text(size=13),
        panel.border    = element_rect(colour = "black", fill=NA, size=2)) 


# Principal Component Analysis (PCA) --------------------------------------

# Load data
data(iris)

# Inspect the data
head(iris)

#Perform PCA
iris.pca <- PCA(iris[,-5], graph = FALSE)

#Plot PCA
c <- fviz_pca_biplot(X       = iris.pca, 
                     col.ind = iris$Species, 
                     palette = "lancet", 
                     addEllipses = TRUE,
                     label   = "var",
                     col.var = "blue", 
                     repel   = TRUE,
                     legend.title = "Species",
                     title = "Análisis de Componentes Principales - Biplot") +
  theme(axis.text.y     = element_text(size=13),
        axis.text.x     = element_text(size=13),
        axis.title.y    = element_text(size=13),
        axis.title.x    = element_text(size=13),
        legend.position = "top",
        panel.border    = element_rect(colour = "black", fill=NA, size=2)) 




# CLUSTER -----------------------------------------------------------------
# Load data
data(USArrests)

# Inspect the data
head(USArrests)

#Treatment data
df <- USArrests
df <- na.omit(df)
df <- scale(df)
head(df)

#Perform kmeans
k5 <- kmeans(df, centers = 5, nstart = 25)

# plots to compare
d <- fviz_cluster(k5, geom = "point",  data = df, main = "Análisis Cluster k = 5") + 
  theme(axis.text.y     = element_text(size=13),
        axis.text.x     = element_text(size=13),
        axis.title.y    = element_text(size=13),
        axis.title.x    = element_text(size=13),
        legend.position = "top",
        panel.border    = element_rect(colour = "black", fill=NA, size=2)) 


# organizing the plot panel and saving
(c | d) / (a | b)
dev.copy(tiff, filename = "statistics.tiff", width = 2100, height = 1700, res = 200)
dev.off()
