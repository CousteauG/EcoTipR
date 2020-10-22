require(plot3D)
require(marmap)
require(musculusColors)


data(nw.atlantic)
atl = as.bathy(nw.atlantic)

colfunc <- colorRampPalette(rev(musculus_palette("Bmsurface", 6)))
#colfunc <- colorRampPalette(c("lightsteelblue4", "lightsteelblue3", "lightsteelblue2", "lightsteelblue1"))
Cols = colfunc(40)

xPos1 = rnorm(n = 20, mean = -72, sd = 0.5)
yPos1 = rnorm(n = 20, mean = 39, sd = 0.5)
zPos1 = rnorm(n = 20, mean = -5, sd = 0.5)
xPos2 = rnorm(n = 20, mean = -65, sd = 0.5)
yPos2 = rnorm(n = 20, mean = 40, sd = 0.5)
zPos2 = rnorm(n = 20, mean = -150, sd = 0.5)
xPos3 = rnorm(n = 20, mean = -55, sd = 0.5)
yPos3 = rnorm(n = 20, mean = 37, sd = 0.5)
zPos3 = rnorm(n = 20, mean = -1200, sd = 0.5)

all_angles = c(20,25,30,35,40,45,50)

for(i in seq_along(all_angles)){
  
png(file=paste0("Example_", i, ".png"), 
    width = 100, height = 100, units = 'mm', res = 350)

  par(mfrow = c(1, 1), mar = c(0.2,0.2,0.2,0.2))
  persp3D(x = as.numeric(rownames(atl)), y = as.numeric(colnames(atl)), z = atl, xlab = "longitude", 
          ylab = "latitude", zlab = "depth", col = Cols, contour = list(side = 'z',col = "gray", nlevels = 35),
          clab = "depth, m", colkey = list(plot = FALSE), theta = all_angles[i], phi = 30)
  points3D(xPos1, yPos1, zPos1, 
           col = "red", size = 15, add=T, pch = 19)
  points3D(xPos2, yPos2, zPos2, 
           col = "green", size = 15, add=T, pch = 19)
  points3D(xPos3, yPos3, zPos3, 
           col = "blue", size = 15, add=T, pch = 19)
  
  dev.off()

}

#Create the animation (GIF):
system("convert2 -delay 80 *.png example_animation.gif")

# to not leave the directory with the single png files
# I remove them.
file.remove(list.files(pattern=".png"))

