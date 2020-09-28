library(maps)
library(fields)
library(ggplot2)
library(devtools)
library(RColorBrewer)
require(gridExtra)

# Data can be found in https://nsidc.org/data/seaice_index/archives

allFiles = list.files(path = 'Data') # Your folder containing all daily files


# Read files:
allData = NULL
for(i in seq_along(allFiles)){
  tmp = read.csv(file.path('Data', allFiles[i]))
  allData = rbind(allData, tmp)
}

allData$area[allData$area == -9999] = NA
allData$extent[allData$extent == -9999] = NA
allData = allData[allData$year > 1978 & allData$year < 2020, ]

# Begin plot:
allYear = sort(unique(allData$year))
for(k in seq_along(allYear)){

tmp_plot = allData[allData$year <= allYear[k], ]
tmp_plot2 = allData[allData$year == allYear[k], ]

p2 =   ggplot(data = tmp_plot, aes(x = mo, y = extent)) +
          geom_line(aes(color = factor(year))) +
          geom_line(data = tmp_plot2, aes(x = mo, y = extent), colour = 'black') +
          coord_fixed() +
          ggtitle(allYear[k]) +
          xlab("Month") +
          ylab("Sea ice extent (million sq km)") +
          theme_bw() +
          scale_color_manual(values = rep('grey80', times = k)) +
          theme(legend.position = "none") +
          scale_x_continuous(breaks = 1:12,
                           labels = 1:12)+ 
          coord_cartesian(ylim=c(3.5, 16.5), xlim = c(1, 12))


png(file=paste0("IceArea_",  allYear[k], ".png"), 
    width = 120, height = 110, units = 'mm', res = 250)
  
  print(p2)

dev.off()

}

# 
# cols <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
# myPal <- cols(length(unique(allData$year)))
# 
# png(file="IceArea_2020.png", 
#     width = 120, height = 110, units = 'mm', res = 250)
# 
# print( ggplot(data = allData, aes(x = mo, y = extent)) +
#           geom_line(aes(color = factor(year))) +
#           coord_fixed() +
#           ggtitle('Integrated') +
#           xlab("Month") +
#           ylab("Ice extent (million sq km)") +
#           theme_bw() +
#           scale_color_manual(values = myPal) +
#           theme(legend.position = 'none') +
#           scale_x_continuous(breaks = 1:12,
#                              labels = 1:12)+ 
#           coord_cartesian(ylim=c(3.5, 16.5), xlim = c(1, 12))
# )
# 
# dev.off()



#Create the animation (GIF):
system("convert2 -delay 80 *.png example_animation.gif")

# to not leave the directory with the single png files
# I remove them.
file.remove(list.files(pattern=".png"))
