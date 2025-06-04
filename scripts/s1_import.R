library(data.table)
library(stringr)
library(lubridate)
library(hms)
library(janitor)

# import datasets --------------------------------------------------------------

prf_apreensoes <- fread("data-raw/PRF_apreensoes_comp.csv") |> clean_names()
#prf_cigarro    <- fread("data-raw/PRF_ApreensoesCigarro.csv") |> clean_names()
#prf_vape       <- fread("data-raw/PRF_ApreensoesVape.csv") |> clean_names()

# Filtrar somente registros do ES
prf_apreensoes <- prf_apreensoes[uf == "ES"]

# Conversão para fatores (variáveis categóricas)
prf_apreensoes[, c("tipo_apreensao", "grupo_apreensao", 
                   "tipo_local_ocorrencia", "zona_ocorrencia", 
                   "municipio_ocorrencia", "uf", "estado") := 
                 lapply(.SD, as.factor), 
               .SDcols = c("tipo_apreensao", "grupo_apreensao", 
                           "tipo_local_ocorrencia", "zona_ocorrencia", 
                           "municipio_ocorrencia", "uf", "estado")]

# Conversão para numérico
prf_apreensoes[, c("quantidade_item", "quantidade_pessoas_detidas") := 
                 lapply(.SD, as.numeric), 
               .SDcols = c("quantidade_item", "quantidade_pessoas_detidas")]

# Substituir vírgula por ponto
prf_apreensoes[, longitude := gsub(",", ".", longitude)]
prf_apreensoes[, latitude := gsub(",", ".", latitude)]

# Converter para numérico
prf_apreensoes[, longitude := as.numeric(longitude)]
prf_apreensoes[, latitude := as.numeric(latitude)]

# Conversão para data
prf_apreensoes[, data_apreensao := parse_date_time(data_apreensao,
                                                   orders = c("dmy", "ymd"))]

# Conversão para hora (hora_apreensao como string padrão HH:MM:SS)
prf_apreensoes[, hora_apreensao := hms::as_hms(hora_apreensao)]
