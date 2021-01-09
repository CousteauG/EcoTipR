rm(list = ls()); gc(reset = TRUE)

# Cargar paquetes necearios
# Load required packages
require(sf)
require(readxl)
require(ggplot2)
require(rayshader)
require(av)
require(viridis)

# Leer datos de Perú por departamento
# Read Peru mapping data by regions
peru1 <- readRDS(file = "gadm36_PER_1_sf.rds")

# Leer archivo de valores de población
# Read file with population values
peruPopulation <- read_excel(path = "datos_INEI.xlsx", sheet = 1)

# Crear un data frame específico por región para el 2017
# Create specific data frame by region for year 2017 
peruPopulation_2017 <- peruPopulation[,c(1, ncol(peruPopulation))]

# Cambiar nombre de columna de valores de población
# Rename population values' column
colnames(peruPopulation_2017)[2] <- "log_population"

# Convertir valores a escala logarírmica
# Convert to logarithmic scale
peruPopulation_2017$log_population <- log10(peruPopulation_2017$log_population)
  
# Combinar fuentes de datos
# Merging data sources
peru1 <- merge(x = peru1, y = peruPopulation_2017, 
               by.x = "NAME_1", by.y = "departamento")

# Definir puntos de corte para barra de colores de leyenda
# Define breaks for legend color bar
zTicks <- log10(c(100000, 300000, 1000000, 3000000, 10000000))

# Iniciar figura simple (ggplot2)
# Initialize simple figure (ggplot2)
classicalPlt <- ggplot(peru1) +
  
  # Añadir regiones coloreadas según valor de población
  # Add region shapes filled by population value
  geom_sf(aes(fill = log_population), colour = "gray90", size = 0.1) +
  
  # Definir límites de longitud
  # Define longitude limits
  scale_x_continuous(limits = c(-85, -65), expand = c(0, 0)) +
  
  # Definir límites de latitud
  # Define latitude limits
  scale_y_continuous(limits = c(-20, 0), expand = c(0, 0)) +
  
  # Modificar algunos aspectos de leyenda
  # Customize some legend features
  scale_fill_continuous(type = "viridis", 
                        breaks = zTicks, 
                        labels = format(x = round(10^zTicks, 0)/1e6, scientific = FALSE), 
                        limits = range(zTicks)) +
  
  # Añadir título y subtítulo de gráfico
  # Add title and subtitle
  ggtitle(label = "Población de Perú al 2017", subtitle = "Fuente: INEI-2020") +
  
  # Definir tema B/N
  # Define B/N theme
  theme_bw() +
  
  # Extender tamaño de barra de color de leyenda
  # Increasing length of color bar scale 
  theme(legend.key.height = unit(2, "cm")) +
  
  # Cambiar título de leyenda
  # Change title for legend title
  labs(fill = "Población\n(millones de invidiuos)") 


# Convertir gráfico de ggplot2 a gráfico 3D
# Convert ggplot2 figure to 3D plot
plot_gg(ggobj = classicalPlt, multicore = TRUE, scale = 150, 
        windowsize = c(1920, 1080), width = 16/2, height = 9/2,
        reduce_size = c(1, 1))

# Animar y guardar gráfico como vídeo en formato mp4
# Animate and save plot as mp4 clip
render_movie(filename = "EcoTipR_17_animation.mp4", fps = 80, frames = 80*10, 
             type = "oscillate", zoom = 0.5)
