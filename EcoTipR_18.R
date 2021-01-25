datos    <- read.csv("trayectoria.csv")
costa    <- read.csv("costa.csv")

library(animation)
saveGIF({
  for(s in 1:length(x)){
    
    #layout(m, widths = c(1,0.20))
    par(mar = c(2.5,2.5,2,1.5), oma = c(0,0,0,0))
    plot(x,y, col = "gray30", type = "l", lty = 2, lwd = 2, cex.axis = 1.5,
         main = datos$DATE.INTERP[s], cex.main = 1.5)

    lines(costa$lon, costa$lat)
    Arrows(x[s], y[s], x[s+4], y[s+4], 
           col = velCol(datos$Vel.Cal[s]), 
           code = 2, lwd = 5)
    
    require(geoR)
    legend.krige(x.leg = c(max(x)-0.75, max(x)-0.3), y.leg = c(-9.05,-9), scale.vals = c(NA,NA,NA),values = 0:20,
                 vertical=F, col=c(2,2,7,7,5,5,5,3,3,3,3,3,3,3,3,3))
    text(x = max(x)-0.52, y = -8.98, labels = "Velocidad (nudos)", cex = 1.2, font = 2)
    text(x = c(-79.35,-79.29,-79.22,-79.08), y = rep(-9.08), 
         labels = c("[0-2]","[2-5]","[5-8]","[8-15]"), cex.lab =  1.2, font = 1)
    
  }  
},movie.name = "example.gif", interval = 0.15, nmax = 15)

