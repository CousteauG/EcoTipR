rm(list = ls()); gc(reset = TRUE)

# Parámetros --------------------------------------------------------------
# Indicar nombre de archivo NetCDF
ncFile <- "data/precipitation/chirps20GlobalMonthlyP05_e470_ead3_db32.nc"

# Indicar nomnre de variable dentro del archivo
ncVar <- "precip"

# Indicar ruta para guardar datos de precipitación por mes como archivo csv
outputFile <- "outputs/precipitation_median_2009-2019.csv"

# Indicar ruta para guardar animación
outputAnimation <- "figures/example_2009-2019.mp4"

# Definir límites de X, Y y Z (precipitación) para el mapa
xlim_map <- c(-90, -30)
ylim_map <- c(-50, 20)
zlim_map <- c(0, 1e3)

# Definir límites de X y Y para las series de tiempo
ylim_ts <- c(0, 1e3)
xlim_ts <- c("2009-1-1", "2019-1-1")

# Definir límites de zonas que se desea analizar
# x: Coordenada X del centroide del polígono
# y: Coordenada Y del centroide del polígono
# side: Ancho del polígono en grados
zones <- list(x = c(-75.46167, -79.76119, -76.82172, -50.98074),
              y = c(-8.883453, -1.709936,  5.463581,  2.888472),
              side = c(1, 1, 1, 1))

# Colores para la paleta en el mapa
cols <- c("darkseagreen1", "gold", "red4")

# Color de la línea de los polígonos
polygonCol <- "blue"

# Color de la línea de las series de tiempo
lineCol <- "dodgerblue4"


# Generación de gráfico ---------------------------------------------------

# Cargar paquetes necesarios
require(ncdf4)
require(raster)
require(mapdata)
require(animation)
require(Hmisc)
require(prettymapr)

# Leer archivo de precipitaciones en formato NetCDF
ncFile <- nc_open(filename = ncFile)
ncData <- ncvar_get(nc = ncFile, varid = ncVar)
nc_close(nc = ncFile)

# Construir la paleta de colores a partir de los colores definidos anteriormente
cols <- colorRampPalette(cols)(1e3)

# Definir límite de eje X de los gráficos de serie de tiempo
xlim_ts <- as.Date(xlim_ts)

# Definir valores completos de tiempo a partir de los metadatos del NetCDF
allDates <- as.Date(as.POSIXlt(ncFile$dim$time$vals, origin = "1970-1-1 00:00:00z"))

# Definir una función wrapper para graficar
png2 <- function(..., res = 250, bg = "transparent") png(..., res = res, bg = bg)

# Definir algunos parámetros de la función de animación
ani.options(ani.height = 1080*1.5, ani.width = 1920*1.5,
            interval = 0.2, nmax = 1e3, ani.dev = png2,
            description = "AFIPDBL-IMARPE-2019", title = "Cromwell Monitoring, Plot 1")

# Iniciar la animación
saveVideo(expr = {
  
  # Definir objetos en blanco para ir guardando los valores de la TS
  output <- data.frame(date = allDates)
  output <- cbind(output, matrix(data = NA, nrow = length(allDates), ncol = length(zones$x)))
  
  # Iniciar un bucle a lo largo de las fechas del NetCDF
  for(i in seq_along(allDates)){
    
    # Definir matriz para el layout (arreglo de figuras)
    layoutMatrix <- matrix(data = c(1, 1, 1, 1, 1, 1, 2, 3, 3, 3, 3, 3, 3,
                                    1, 1, 1, 1, 1, 1, 2, 4, 4, 4, 4, 4, 4,
                                    1, 1, 1, 1, 1, 1, 2, 5, 5, 5, 5, 5, 5,
                                    1, 1, 1, 1, 1, 1, 2, 6, 6, 6, 6, 6, 6), 
                           nrow = 4, byrow = TRUE)
    
    # Definir layout
    layout(mat = layoutMatrix)
    
    
    ### MAPA ###
    # Definir parámetros de gráfico
    par(mar = rep(0, 4), oma = c(2, 4, 1, 4))
    
    # Convertir matriz de datos en una lista tipo XYZ
    rasterData <- list(x = ncFile$dim$longitude$vals,
                       y = ncFile$dim$latitude$vals,
                       z = ncData[,,i])
    
    # Dibujar raster de precipitaciones
    image(raster(rasterData), axes = FALSE, xlim = xlim_map, ylim = ylim_map, zlim = zlim_map,
          col = cols, xlab = NA, ylab = NA)
    
    # Dibujar bordes geográficos
    map("worldHires", add = TRUE, interior = FALSE)
    
    # Añadir texto de año-mes
    mtext(text = capitalize(format(x = allDates[i], format = "%B %Y")), side = 3, 
          line = -2, adj = 0.99, font = 2, cex = 1.2)
    
    # Añadir texto de fuente de información satelital
    mtext(text = "CHIRPS Version 2.0, Precipitation, Global, 0.05°, Monthly", 
          side = 1, line = -1, adj = 0.99, cex = 0.5)
    
    # Añadir flecha de Norte
    addnortharrow(pos = "bottomleft", padin = c(0.10, 0.10), scale = 0.45,lwd = 1,
                  border = "black", cols = c("white", "black"),text.col = "black")
    
    # Iniciar un bucle a lo largo de las zonas definidas
    for(j in seq_along(zones$x)){
      
      # Definir valores de polígonos cuadrados
      xValues <- zones$x[j] + zones$side[j]/2*c(-1, 1)
      yValues <- zones$y[j] + zones$side[j]/2*c(-1, 1)
      
      # Dibujar polígono
      polygon(x = rep(xValues, each = 2), y = yValues[c(1, 2, 2, 1)], border = polygonCol,
              lwd = 2)
      
      # Añadir nombre de polígono en números romanos
      text(x = xValues[2], y = mean(yValues), labels = as.character(as.roman(j)), 
           pos = 4, font = 2)
    }
    
    # Definir valores de ejes X y Y
    xAxis <- seq(xlim_map[1], xlim_map[2], 10)
    yAxis <- seq(ylim_map[1], ylim_map[2], 10)
    
    # Dibujar etiquetas de X y Y, añadiendo, según corresponda, W-S o S-N
    axis(side = 1, at = xAxis, 
         labels = paste0(abs(xAxis), as.character(factor(sign(xAxis), c(-1, 0, 1), c("° W", "°", "° E")))))
    axis(side = 2, at = yAxis, las = 1, 
         labels = paste0(abs(yAxis), as.character(factor(sign(yAxis), c(-1, 0, 1), c("° S", "°", "° N")))))
    
    # Dibujar cuadro alrededor de mapa
    box()
    
    
    ### LEYENDA DE MAPA ###
    # Definir parámetros de gráfico
    par(mar = c(0, 0.5, 0, 4))
    
    # Definir vector con valores de leyenda
    legendValues <- seq(from = zlim_map[1], to = zlim_map[2],  length.out = 1e3)
    
    # Dibujar leyenda a modo de barra de colores 
    image(matrix(data = legendValues, nrow = 1), col = cols, axes = FALSE)
    
    # Definir etiquetas de leyenda
    legendLabels <- seq(from = zlim_map[1], to = zlim_map[2], by = 200)
    
    # Dibujar etiquetas de leyenda
    axis(side = 4, at = seq(from = 0, to = 1, length.out = length(legendLabels)),
         labels = legendLabels, las = 1)
    
    # Añadir nombre de variable
    mtext(text = "Precipitación (mm/mes)", side = 4, line = 3)
    
    # Dibujar cuadro alrededor de barra de colores
    box()
    
    
    ### SERIES DE TIEMPO POR ZONA ###
    # Definir parámetros de gráfico
    par(mar = c(0, 4, 0, 0), xaxs = "i", yaxs = "i")
    
    # Iniciar bucle a lo largo de las TS definidas
    for(j in seq_along(zones$x)){
      
      # Definir límites de polígono
      xValues <- zones$x[j] + zones$side[j]/2*c(-1, 1)
      yValues <- zones$y[j] + zones$side[j]/2*c(-1, 1)
      
      # Concatenar límites como una tabla XY
      coords <- cbind(x = xValues[c(1, 1, 2, 2, 1)],
                      y = yValues[c(1, 2, 1, 2, 1)])
      
      # Convertir tabla en un polígono (shape)
      tempPolygon <- SpatialPolygons(list(Polygons(list(Polygon(coords)), 1)))
      
      # Extraer mediana valores de variable (precipitación) dentro del polígono
      tempMedian <- median(extract(x = raster(rasterData), y = tempPolygon)[[1]], 
                           na.rm = TRUE)
      
      # Asignar valor de mediana en la casilla correspondiente
      output[i, j + 1] <- tempMedian
      
      # Dibujar lienzo vacío
      plot(1, 1, type = "n", axes = FALSE, xlab = NA, ylab = NA, 
           xlim = xlim_ts, ylim = ylim_ts)
      
      # Añadir líneas de serie de tiempo acumulada al mes respectivo
      lines(x = output$date, y = output[,j + 1], lwd = 2, col = lineCol)
      
      # Añadir etiquetas de eje Y, ya sea a la izquierda(2) o a la derecha (4)
      axis(side = ifelse(j %% 2 == 0, 4, 2), at = seq(ylim_ts[1], ylim_ts[2], 200), 
           las = 1)
      
      # Añadir nombre de zona en la esquina superior derecha
      mtext(text = as.character(as.roman(j)), side = 3, line = -2, adj = 0.99, 
            font = 2)
      
      # Dibujar cuadro alrededor de barra de la figura de TS
      box()
    }
    
    # Añadir eje X de la TS: solo separadores vcada seis meses
    axis(side = 1, at = seq(from = xlim_ts[1], to = xlim_ts[2], by = "6 month"), 
         labels = NA, tcl = -0.25)
    
    # Añadir eje X de la TS: separador y etiquetas cada año
    xLabs <- seq(from = xlim_ts[1], to = xlim_ts[2], by = "year")
    axis(side = 1, at = xLabs, labels = format(xLabs, format = "%Y"))
  }
  
  # Guardar valores como un archivo csv
  write.csv(x = output, file = outputFile, row.names = FALSE, na = "")
}, video.name = outputAnimation, 
ffmpeg = "C:/Program Files/FFmpeg/bin/ffmpeg.exe")
