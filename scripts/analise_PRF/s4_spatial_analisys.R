library(leaflet)
library(data.table)
library(ggplot2)
library(spatstat)
library(raster)
library(leaflet.extras)

# import data ------------------------------------------------------------------

data <- fread("data/prf_apreensoes.csv")

## Filtragens para as analises apenas do estado presente e 
data <- data[uf == "ES"]

data[, (c("uf", "estado")) := NULL]

municipios_estado_presente <- c("ARACRUZ", "CACHOEIRO DE ITAPEMIRIM", "CARIACICA",
                                "COLATINA", "LINHARES", "SAO MATEUS", "SERRA",
                                "VILA VELHA", "VITORIA")

data_corte <- as.Date("2017-12-28")

data[, programa := ifelse(data_apreensao >= data_corte, 1, 0)]

data[, ano := year(data_apreensao)]

data[, ano_mes := format(data_apreensao, "%Y-%m")]

data[, tempo := as.numeric(factor(ano_mes))]  # Converte ano_mes em 1, 2, 3...

data[, pos_intervencao := fifelse(
  programa == 1, 
  tempo - min(tempo[programa == 1]),
  0
)]

## Criar variável dummy para o Estado Presente
data[,estado_presente := factor(ifelse(municipio_ocorrencia %in% municipios_estado_presente,
                                       "Estado Presente", "Demais municípios"))]
# Mapa de Hotspots -------------------------------------------------------------

## Converter para objeto ppp (spatstat)

coord <- data[, .(longitude, latitude, programa)]
w <- owin(range(coord$longitude), range(coord$latitude))

coord_agg <- coord[, .(n = .N), by = .(longitude, latitude, programa)]

## Densidade por período
dens_antes <- density(ppp(coord_agg[programa == 0, longitude], 
                          coord_agg[programa == 0, latitude], 
                          weights = coord_agg[programa == 0, n],
                          window = w))
dens_depois <- density(ppp(coord_agg[programa == 1, longitude], 
                          coord_agg[programa == 1, latitude], 
                          weights = coord_agg[programa == 1, n],
                          window = w))

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



library(sf)
library(tidyverse)
library(viridis)
library(ggspatial)

# 1. Carregar o shapefile dos bairros
# Substitua "caminho/para/seu/arquivo.shp" pelo caminho real do seu shapefile
shape<- st_read("data/shapefiles/ES_UF_2024.shp")


# 3. Converter os dados de apreensão para objeto sf (espacial)
# Certifique-se que as colunas de latitude e longitude estão corretas
dados_sf <- st_as_sf(data, 
                     coords = c("longitude", "latitude"), 
                     crs = st_crs(shape))

# Opção A: Contagem simples
contagem_municipios <- st_join(shape, dados_sf) %>%
  group_by(municipio_ocorrencia) %>% # Ajuste para os nomes das colunas no seu shapefile
  summarise(total_apreensoes = sum(total_apreensoes, na.rm = TRUE)) %>%
  ungroup()


# 5. Criar o mapa de calor
ggplot() +
  # Plotar os bairros com preenchimento baseado no total de apreensões
  geom_sf(data = contagem_municipios, aes(fill = total_apreensoes), color = NA) +
  # Escala de cores (usando viridis para melhor visualização)
  scale_fill_viridis(name = "Total de Apreensões", option = "plasma", trans = "sqrt") +
  # Título e legendas
  labs(title = "Mapa de Calor de Apreensões por Municipio",
       subtitle = "Dados da PRF",
       caption = "Fonte: Polícia Rodoviária Federal") +
  # Elementos adicionais do mapa
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", which_north = "true") +
  # Tema minimalista
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank())

# 6. (Opcional) Salvar o mapa
ggsave("mapa_apreensoes_bairros.png", width = 10, height = 8, dpi = 300)



# carregar shapefile ----------------------------------------------------------

es_shp <- st_read("data/shapefiles/ES_UF_2024.shp") # atualize para o caminho correto

# converter dados de apreensões para sf ---------------------------------------

apreensoes_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)
apreensoes_sf <- st_transform(apreensoes_sf, st_crs(es_shp)) # para combinar com shapefile

# filtrar apenas os pontos dentro do shapefile do ES --------------------------

apreensoes_sf <- apreensoes_sf[st_within(apreensoes_sf, es_shp, sparse = FALSE), ]

# converter shapefile para janela espacial (spatstat) -------------------------

es_sp <- as(es_shp, "Spatial")
janela <- as.owin(es_sp)

# converter os pontos para objeto ppp -----------------------------------------

coords <- st_coordinates(apreensoes_sf)
ppp_obj <- ppp(x = coords[,1], y = coords[,2], window = janela)

# estimar densidade kernel ----------------------------------------------------

densidade <- density.ppp(ppp_obj, sigma = 2000) # ajuste sigma conforme necessário

# converter densidade para raster e depois data.frame -------------------------

r <- rast(densidade)
r_df <- as.data.frame(r, xy = TRUE)
names(r_df)[3] <- "densidade"

# gráfico ---------------------------------------------------------------------

ggplot() +
  geom_sf(data = es_shp, fill = NA, color = "gray20") +
  geom_raster(data = r_df, aes(x = x, y = y, fill = densidade), alpha = 0.8) +
  scale_fill_viridis(name = "Densidade", option = "inferno") +
  coord_sf() +
  labs(
    title = "Hotspot de Apreensões no Espírito Santo",
    subtitle = "Estimativa de densidade kernel das ocorrências",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14)
  )



# Coordenadas dos pontos
coords <- st_coordinates(apreensoes_sf)

# Adiciona as coordenadas como colunas separadas para facilitar uso no ggplot
apreensoes_sf$lon <- coords[, 1]
apreensoes_sf$lat <- coords[, 2]

# Gráfico
ggplot() +
  geom_sf(data = es_shp, fill = "white", color = "gray60") +
  stat_density_2d(
    data = apreensoes_sf,
    aes(x = lon, y = lat, fill = after_stat(level)),
    geom = "polygon",
    alpha = 0.6,
    bins = 30
  ) +
  scale_fill_viridis_c(option = "inferno", name = "Densidade") +
  geom_point(data = apreensoes_sf, aes(x = lon, y = lat), color = "black", size = 0.5, alpha = 0.3) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.3, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) +
  labs(title = "Hotspot de Apreensões no Espírito Santo",
       subtitle = "Densidade Kernel das ocorrências registradas",
       x = "Longitude", y = "Latitude") +
  theme_minimal()