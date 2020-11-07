library(leaflet)
library(leaflet.extras)

# Data can be found in
# https://idesep.senamhi.gob.pe/geovisoridesep

map <- leaflet() %>%
  addTiles(group = "Open Street Map") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
  setView(lng = -75, lat = -10, zoom = 5) %>%
  addWMSTiles(
    "http://idesep.senamhi.gob.pe/geoserver/g_02_01/wms?",
    layers = "g_02_01:02_01_001_03_001_512_1983_00_00",
    options = WMSTileOptions(format = "image/png", transparent = TRUE),
    attribution = "SENAMHI",
    group = "Precipitación 83"
  ) %>%
  addWMSTiles(
    "http://idesep.senamhi.gob.pe/geoserver/g_02_02/wms?",
    layers = "g_02_02:02_02_001_03_001_512_1998_00_00",
    options = WMSTileOptions(format = "image/png", transparent = TRUE),
    attribution = "SENAMHI",
    group = "Precipitación 98"
  ) %>%
  addWMSTiles(
    "http://idesep.senamhi.gob.pe/geoserver/g_02_04/wms?",
    layers = "g_02_04:02_04_001_03_001_512_2017_00_00",
    options = WMSTileOptions(format = "image/png", transparent = TRUE),
    attribution = "SENAMHI",
    group = "Precipitación 2017"
  ) %>%
  addWMSLegend(uri = "http://idesep.senamhi.gob.pe/geoserver/g_02_01/wms?REQUEST=GetLegendGraphic&VERSION=1.0.0&FORMAT=image/png&WIDTH=20&HEIGHT=20&LAYER=g_02_01:02_01_001_03_001_512_1983_00_00",
               position = "bottomright") %>%
  addScaleBar(position = "bottomleft") %>%
  addLayersControl(
    baseGroups = c("Open Street Map", "Esri World Imagery"),
    overlayGroups = c("Precipitación 83", "Precipitación 98", "Precipitación 2017"),
    position = "topright",
    options = layersControlOptions(collapsed = TRUE)
  ) %>%
  hideGroup(group = c("Precipitación 98", "Precipitación 2017"))
map
