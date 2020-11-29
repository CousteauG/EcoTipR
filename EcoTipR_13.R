# -------------------------------------------------------------------------
# Installing packages -----------------------------------------------------
# -------------------------------------------------------------------------
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("ggpubr")) install.packages("ggpubr")
if(!require("colorspace")) install.packages("colorspace")
if(!require("wesanderson")) install.packages("wesanderson")
if(!require("ggrepel")) install.packages("ggrepel")
if(!require("patchwork")) install.packages("patchwork")
library(ggplot2)
library(patchwork)
library(wesanderson)
library(colorspace)
library(ggpubr)
library(ggrepel)


# -------------------------------------------------------------------------
# Regression | boxplot | density plots ------------------------------------
# -------------------------------------------------------------------------

# Load data
data("mtcars")
data("iris")

# Inspect the data
head(mtcars)
head(iris)

# Convert cyl as a grouping variable
mtcars$cyl <- as.factor(mtcars$cyl)

#REGRESSION PLOT
p1 <- ggscatter(mtcars, x = "wt", y = "mpg",
                add = "reg.line",                         # Add regression line
                conf.int = TRUE,                          # Add confidence interval
                color = "cyl", palette = "jco",           # Color by groups "cyl"
                shape = "cyl") +                          # Change point shape by groups "cyl"
  stat_cor(aes(color = cyl,
               label = paste(..rr.label.., cut(..p.., 
                                               breaks = c(-Inf, 0.0001, 0.001, 0.01, 0.05, Inf),
                                               labels = c("'****'", "'***'", "'**'", "'*'", "'ns'")), 
                             sep = "~")), digits = 3, label.x = 4, size = 5) +
  theme(axis.text.y   = element_text(size=14),
        axis.text.x   = element_text(size=14),
        axis.title.y  = element_text(size=14),
        axis.title.x  = element_text(size=14),
        panel.border  = element_rect(colour = "black", fill=NA, size=2),
        legend.title  = element_text(colour ="blue", size = 14, face="bold"),
        legend.text   = element_text(colour="blue", size = 14, face="bold")) +
  labs(y = "Miles/(US) gallon", x = "Weight (1000 lbs)")

# BOXPLOT
p2 <- ggboxplot(mtcars, x = "cyl", y = "mpg",
                color = "cyl",
                shape = "cyl",
                palette = wes_palette("Darjeeling1"), 
                add = "jitter", width = 0.25) +
  labs(y = "Miles/(US) gallon", x = "Weight (1000 lbs)")

# DENSITY PLOT
p3 <- ggdensity(iris, x = "Sepal.Length", 
                color = "Species", 
                fill = "Species", 
                palette = wes_palette("Darjeeling1"), add ="mean") +
  labs(y="Density", x = "Sepal length (cm)")

# organizing the plot panel and saving
(p1 | p2)/p3
dev.copy(tiff, filename = "ggpubr.tiff",width = 2100, height = 1700, res = 200)
dev.off()