# Load packages
# Cargar paquetes
require(ggplot2)
require(gganimate)
require(sf)
require(rnaturalearth)
require(rnaturalearthdata)

# Read points table
# Leer tabla de puntos
read.csv(file = "allPoints.csv")

# Extract polygon for South America 
# Extraer polígonp para Sudamérica
southamerica <- ne_countries(scale = "large", continent = "South America", returnclass = "sf")

# Start drawing
# Iniciar figura
myAnimation <- ggplot(southamerica) +
  
  # Add land polygon
  # Añadir polígono de tierra
  geom_sf(fill = "burlywood1") +
  
  # Define limits for X and Y
  # Definir límites X y Y
  coord_sf(xlim = c(-90, -70), ylim = c(-20, -2), expand = FALSE) +
  
  # Add points
  # Añadir puntos
  geom_point(data = allPoints, aes(x = x, y = y, colour = type), show.legend = FALSE, alpha = 0.7) +
  
  # Remove X and Y axis labels 
  # Quitar títulos de ejes X y Y
  xlab(NULL) + ylab(NULL) + 
  
  # Set background color 
  # Definir color de fondo
  theme(panel.background = element_rect(fill = "powderblue")) +
  
  # Define transition variable
  # Definir variable para las transiciones
  transition_time(z) +
  
  # Add title for time steps
  # Añadir título de pasos de tiempo
  labs(title = "Time: {frame_time}") +
  
  # Add shadow effect between time steps
  # Añadir efecto de sombra entre cada paso de tiempo
  shadow_wake(wake_length = 0.1, alpha = FALSE)

# Save animation
# Guardar animación
anim_save(filename = "animation1.mp4", animation = myAnimation, path = "D:/",
          fps = 10, device = "png", width = 1000, height = 900, renderer = ffmpeg_renderer(),
          res = 150)
