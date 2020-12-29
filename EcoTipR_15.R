rm(list = ls()); gc(reset = TRUE)

# Cargar paquetes requeridos
require(ggplot2)
require(reshape2)

# Cargar datos de calidad de aire
data("airquality")

# Convertir variable Month a clase factor
airquality$Month <- factor(x = airquality$Month, 
                           levels = sort(unique(airquality$Month)), 
                           labels = month.name[sort(unique(airquality$Month))])

# Figura 1: boxplots
plot_1 <- airquality %>% ggplot() + 
  
  # Dibujar boxplots
  geom_boxplot(aes(x = Month, y = Temp, fill = Month), 
               na.rm = TRUE) +

  # Cambiar a tema B/N
  theme_bw() +
  
  # Mover la posición de la leyenda
  theme(legend.position = "top")


# Figura 1: gráficos de densidad
plot_2 <- airquality %>% ggplot() + 
  
  # Dibujar gráficos de densidad
  geom_density(aes(x = Temp, fill = Month), alpha = 0.5, position = "identity") +
  
  # Cambiar a tema B/N
  theme_bw() +
  
  # Mover la posición de la leyenda
  theme(legend.position = "top")



# Obtener tabla con valores de R2
corMatrix <- melt(data = cor(airquality[,1:4], method = "spearman", use = "na.or.complete"), 
                  value.name = "R2")

# Figura 3: gráfico de matriz de correlaciones
plot_3 <- ggplot(data = corMatrix, aes(x = Var1, y = Var2, fill = value)) + 

  # Dibujar matriz de correlaciones 
  geom_tile() + 
  
  # Remover títulos de ejes
  xlab("") + ylab("") +
  
  # Definir colores de paleta de leyenda
  scale_fill_gradient2(limits = c(-1, 1), low = "blue", high = "red", na.value = "black", name = "") +
  
  # Remover espacios al inicio y final de eje X
  scale_x_discrete(expand = c(0, 0)) +
  
  # Remover espacios al inicio y final de eje Y
  scale_y_discrete(expand = c(0, 0)) +
  
  # Cambiar a tema B/N
  theme_bw() +
  
  # Mover la posición de la leyenda
  theme(legend.position = "bottom", legend.key.width = unit(2, 'cm'))


# Organizar figuras una debajo de la otra
plot_1 / plot_2 / plot_3
