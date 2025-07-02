library(leaflet)
library(data.table)
library(ggplot2)
library(spatstat)

# 1. Preparar os dados ----
data[, data_apreensao := as.IDate(data_apreensao)]
data[, periodo := ifelse(data_apreensao < as.Date("2017-12-28"), "Antes", "Depois")]

# Filtrar coordenadas válidas
dados_mapa <- data[!is.na(latitude) & !is.na(longitude)]

# 2. Análise de Densidade ----
# Converter para objeto ppp (spatstat)
coord <- dados_mapa[, .(longitude, latitude, periodo)]
w <- owin(range(coord$longitude), range(coord$latitude))

# Densidade por período
dens_antes <- density(ppp(coord[periodo == "Antes", longitude], 
                          coord[periodo == "Antes", latitude], window = w))
dens_depois <- density(ppp(coord[periodo == "Depois", longitude], 
                           coord[periodo == "Depois", latitude], window = w))

# 3. Mapa Interativo ----
pal <- colorNumeric("YlOrRd", domain = c(0, max(dens_antes$v, dens_depois$v)))

leaflet() %>%
  addTiles() %>%
  # Hotspots Antes
  addRasterImage(raster(dens_antes), 
                 colors = pal, 
                 opacity = 0.6,
                 group = "Antes (2015-2017)") %>%
  # Hotspots Depois
  addRasterImage(raster(dens_depois), 
                 colors = pal, 
                 opacity = 0.6,
                 group = "Depois (2018-2023)") %>%
  # Controles
  addLayersControl(
    baseGroups = c("Antes (2015-2017)", "Depois (2018-2023)"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegend(pal = pal, 
            values = c(0, max(dens_antes$v, dens_depois$v)),
            title = "Intensidade de Hotspots") %>%
  addScaleBar()
