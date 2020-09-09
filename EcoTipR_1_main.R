# Cousteau Consultant Group -----------------------------------
# Main script

## LOAD LIBRARIES
library(rmarkdown)
library(tinytex)
library(knitr)
library(ncdf4)
library(maps)
library(fields)
library(pracma)
library(raster)
library(MODIS)
require(splancs)
source('EcoTipR_1_aux_fun.R')

#SET YOUR WD FIRST!

# Here your data
myData = expand.grid(YEAR = 2003:2019, MONTH = 1, DAY = 1)

# Paste year, month, and day info:
myData$time = paste0(myData$YEAR, '-', myData$MONTH, '-', myData$DAY)

# Let's find how many oceanographic files we need to download:
uniqueDates = unique(myData$time)


# Define what variable we want to download and the longitude and latitude ranges:
ObjVar = 'chlor_a' # only for chl-a
LongRange = c(-83, -77)
LatiRange = c(-12, -3)

# Begin loop:
for(i in seq_along(uniqueDates)){
  
  # This function get the URL to download the oceanographic data:
  # Lons and Lats are the ranges
  # timeResolution might be 'MO' (monthly) and 'DAY' (daily)
  url = getURL(Var = ObjVar, Lons = LongRange, Lats = LatiRange, 
               iDate = uniqueDates[i])
  
  # Open data:
  sst.nc<-nc_open(url) # download here
  
  # Let's continue:
  # Get longitude, latitude and time values
  fillvalue<-ncatt_get(sst.nc,ObjVar,"_FillValue")
  start.time<-ncatt_get(sst.nc,0,'time_coverage_start')
  end.time<-ncatt_get(sst.nc,0,'time_coverage_end')
  lon<-ncvar_get(sst.nc,varid='lon')
  lat<-ncvar_get(sst.nc,varid='lat')
  
  #Extract sst and remove fill in values (set to NA)
  sst<-ncvar_get(sst.nc,varid=ObjVar)
  sst[sst==fillvalue$value]<-NA
  
  #Close netCDF connection (Important!!)
  nc_close(sst.nc)
  
  png(file=paste0("example_",  myData$YEAR[i], ".png"), 
      width = 150, height = 140, units = 'mm', res = 400)
  
  par(mar = c(4,4,1,4))
  plot.new()
  image.plot(lon,sort(lat),log(sst[,order(lat)]), zlim = c(-2, 5), 
             main=myData$YEAR[i], 
             ylab='latitude',
             xlab='longitude', legend.lab = expression(paste("log(mg/m"^3,')')))
  map("world",fill=T,col="lightgrey",add=T)
  dev.off()
  
  
}

#Create the animation (GIF):
system("convert2 -delay 80 *.png example_animation.gif")

# to not leave the directory with the single png files
# I remove them.
file.remove(list.files(pattern=".png"))


