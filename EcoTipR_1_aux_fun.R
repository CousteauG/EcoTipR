getURL = function(Var, Lons, Lats, Each = 1, Res = '4km', iDate){

    timeResolution = 'YR'
    date_1 = as.Date(iDate, "%Y-%m-%d")
    Yr = format(date_1, format="%Y")
    iniDate = paste0(Yr, '-01-01')
    endDate = paste0(Yr, '-12-31')
    Dates = transDate(begin = iniDate, end = endDate)
    YearNumber = paste0(Dates$beginDOY, Dates$endDOY)
 
  
  
  Year1 = strftime(x = strptime(x = Dates$begin, format = '%Y-%m-%d'), format = '%Y')
  Number1 = substr(x = Dates$beginDOY, start = 5, stop = 7)
  
  if(Res == '4km'){
    
    maxIndLat = 4319
    maxIndLon = 8639
    
  }
  
  if(Res == '9km'){
    
    maxIndLat = 2159
    maxIndLon = 4319
    
  }
  
  if(Var == 'sst') Var2 = 'SST'
  if(Var == 'chlor_a') Var2 = 'CHL'
  
  
  
  FakeLat = c(-90,90) + 90
  LatInd2 = maxIndLat - (floor(((Lats[1]+90)*maxIndLat)/max(FakeLat)))
  LatInd1 = maxIndLat - (ceiling(((Lats[2]+90)*maxIndLat)/max(FakeLat)))
  
  FakeLon = c(-180,180) + 180
  LonInd1 = floor(((Lons[1]+180)*maxIndLon)/max(FakeLon))
  LonInd2 = ceiling(((Lons[2]+180)*maxIndLon)/max(FakeLon))
  
  StrLat = paste0('[', LatInd1, ':', Each, ':', LatInd2, ']')
  StrLon = paste0('[', LonInd1, ':', Each, ':', LonInd2, ']')
  
  StrVar = paste0(StrLat, StrLon)
  
  
  outURL = paste0('https://oceandata.sci.gsfc.nasa.gov:443/opendap/MODISA/L3SMI/',
                  Year1, '/',
                  Number1, '/',
                  'A', YearNumber,
                  '.L3m_', timeResolution, '_', Var2, '_', Var, '_',
                  Res, '.nc?', Var, StrVar, ',lat', StrLat, ',lon', StrLon)
  
  return(outURL)
  
}



numberOfDays <- function(date) {
  m <- format(date, format="%m")
  
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  
  return(as.integer(format(date - 1, format="%d")))
}


