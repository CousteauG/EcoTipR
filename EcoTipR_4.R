library(maps)
library(fields)
library(ggplot2)
library(devtools)
#devtools::install_github("dawnbarlow/musculusColors")
library(musculusColors)
require(gridExtra)

# Data can be found in https://globalfishingwatch.org/data-download/datasets/public-fisshing-effort-10:v20200316

allFiles = list.files(path = 'Daily_data') # Your folder containing all daily files
minLon = -130
maxLon = -85
minLat = 0
maxLat = 30

# Read files:
allData = NULL
for(i in seq_along(allFiles)){
  tmp = read.csv(file.path('Daily_data', allFiles[i]))
  tmp$lat_bin_dec = tmp$lat_bin/10
  tmp$lon_bin_dec = tmp$lon_bin/10
  tmp = tmp[tmp$lat_bin_dec < maxLat & tmp$lat_bin_dec > minLat & tmp$lon_bin_dec < maxLon & tmp$lon_bin_dec > minLon,]
  allData = rbind(allData, tmp)
}

allData$date2 = as.Date(x = allData$date, format = '%Y-%m-%d')
allData$timeindex = format(allData$date2, '%Y%m')

#aggregate data:
plotdata = aggregate(allData$fishing_hours, list(long = allData$lon_bin_dec, lat = allData$lat_bin_dec, timeindex = allData$timeindex), FUN = sum)
plotdata$logx = log(plotdata$x)
world_map <- map_data("world")

# Begin plot:
allMonthYear = unique(plotdata$timeindex)
for(k in seq_along(allMonthYear)){

tmp_plot = plotdata[plotdata$timeindex == allMonthYear[k], ]

# p1 = ggplot() +
#         geom_polygon(aes(x = long, y = lat, group = group), data = world_map, fill = "grey80", color = "grey80") +
#         geom_point(data = tmp_plot, aes(x = long, y = lat, colour = x)) +
#         scale_colour_gradientn(colours = musculus_palette("Bmlunge", n=50), limits=c(0,561)) + #c(-5.3,7)
#         coord_fixed() +
#         ggtitle( allMonthYear[k]) +
#         xlab("longitude") +
#         ylab("latitude") +
#         coord_cartesian(ylim=c(minLat, maxLat), xlim = c(minLon, maxLon)) +
#         theme_bw() +
#         labs(colour = "Fishing hours") +
#         theme(legend.position = c(0.68, 0.82),
#               legend.background = element_rect(fill = "transparent"),
#               legend.title = element_text(size = 8), 
#               legend.text  = element_text(size = 7.5),
#               legend.key.size = unit(0.7, "lines"))

p2 =   ggplot(data = tmp_plot, aes(x = long, y = lat)) +
          geom_point(data = tmp_plot, aes(x = long, y = lat), colour = 'grey40') +
          stat_density_2d(aes(fill = stat(level)),
                          geom = "polygon", 
                          n = 250 ,
                          bins = 10, show.legend = FALSE)+
          coord_fixed() +
          ggtitle(allMonthYear[k]) +
          scale_fill_gradientn(colours = alpha(colour = musculus_palette("Bmlunge", n=50),alpha = 0.3))+
          xlab("longitude") +
          ylab("latitude") +
          coord_cartesian(ylim=c(minLat, maxLat), xlim = c(minLon, maxLon)) +
          theme_bw() +
          geom_polygon(aes(x = long, y = lat, group = group), data = world_map, fill = "grey85", color = "grey85") 


png(file=paste0("GFW_",  allMonthYear[k], ".png"), 
    width = 120, height = 110, units = 'mm', res = 250)
  
  #grid.arrange(p1, p2, nrow = 1)
  print(p2)

dev.off()

}

#Create the animation (GIF):
system("convert2 -delay 80 *.png example_animation.gif")

# to not leave the directory with the single png files
# I remove them.
file.remove(list.files(pattern=".png"))

        

    

