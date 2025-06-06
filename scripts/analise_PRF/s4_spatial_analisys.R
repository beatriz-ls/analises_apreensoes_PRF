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
shape_bairros <- st_read("caminho/para/seu/arquivo.shp")

# 2. Carregar os dados de apreensão
# Substitua pelo seu dataframe ou pelo caminho do arquivo
dados_apreensao <- read.csv("dados_apreensao.csv") # ou seu formato de dados

# Verificar a estrutura dos dados
head(dados_apreensao)

# 3. Converter os dados de apreensão para objeto sf (espacial)
# Certifique-se que as colunas de latitude e longitude estão corretas
dados_sf <- st_as_sf(dados_apreensao, 
                     coords = c("longitude", "latitude"), 
                     crs = st_crs(shape_bairros))

# 4. Contar apreensões por bairro
# Fazer a interseção entre pontos e polígonos dos bairros
contagem_por_bairro <- st_join(shape_bairros, dados_sf) %>%
  as.data.frame() %>%
  group_by(COD_BAIRRO, NOME_BAIRRO) %>% # Substitua pelos nomes reais das colunas do seu shapefile
  summarise(total_apreensoes = n()) %>%
  left_join(shape_bairros, by = c("COD_BAIRRO", "NOME_BAIRRO")) %>% # Ajuste os nomes
  st_as_sf()

# 5. Criar o mapa de calor
ggplot() +
  # Plotar os bairros com preenchimento baseado no total de apreensões
  geom_sf(data = contagem_por_bairro, aes(fill = total_apreensoes), color = NA) +
  # Escala de cores (usando viridis para melhor visualização)
  scale_fill_viridis(name = "Total de Apreensões", option = "plasma", trans = "sqrt") +
  # Título e legendas
  labs(title = "Mapa de Calor de Apreensões por Bairro",
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
