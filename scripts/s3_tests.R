library(data.table)
library(viridis)
library(highcharter)
library(dplyr)

# import data ------------------------------------------------------------------

data <- fread("data/prf_apreensoes.csv")

## Filtragens para as analises apenas do estado presente e 
data <- data[uf == "ES"]

data[, (c("uf", "estado")) := NULL]

municipios_estado_presente <- c("ARACRUZ", "CACHOEIRO DE ITAPEMIRIM", "CARIACICA", "COLATINA",
                                "LINHARES", "SAO MATEUS", "SERRA", "VILA VELHA", "VITORIA")

data_corte <- as.Date("2017-12-28")

data[, programa := ifelse(data_apreensao >= data_corte, 1, 0)]

data[, ano := year(data_apreensao)]

data[, ano_mes := format(data_apreensao, "%Y-%m")]

# Criar variável dummy para o Estado Presente
data[,estado_presente := factor(ifelse(municipio_ocorrencia %in% municipios_estado_presente,
                                       "Estado Presente", "Demais municípios"))]

# Teste de tendência pré-Programa (apenas cidades do estado presente) ----------

