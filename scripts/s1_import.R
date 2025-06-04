library(data.table)
library(stringr)
library(lubridate)
library(hms)
library(janitor)

# import datasets --------------------------------------------------------------

prf_apreensoes <- fread("data-raw/PRF_apreensoes_comp.csv") |> clean_names()
#prf_cigarro    <- fread("data-raw/PRF_ApreensoesCigarro.csv") |> clean_names()
#prf_vape       <- fread("data-raw/PRF_ApreensoesVape.csv") |> clean_names()

## Conversão para fatores (variáveis categóricas)
prf_apreensoes[, c("tipo_apreensao", "grupo_apreensao", 
                   "tipo_local_ocorrencia", "zona_ocorrencia", 
                   "municipio_ocorrencia", "uf", "estado") := 
                 lapply(.SD, as.factor), 
               .SDcols = c("tipo_apreensao", "grupo_apreensao", 
                           "tipo_local_ocorrencia", "zona_ocorrencia", 
                           "municipio_ocorrencia", "uf", "estado")]

## Conversão para numérico
prf_apreensoes[, c("quantidade_pessoas_detidas") := 
                 lapply(.SD, as.numeric), 
               .SDcols = c("quantidade_pessoas_detidas")]

## 
prf_apreensoes[, quantidade_item := gsub(",", ".", quantidade_item)]
prf_apreensoes[, c("quantidade_item") := 
                 lapply(.SD, as.numeric), 
               .SDcols = c("quantidade_item")]

## Substituir vírgula por ponto
prf_apreensoes[, longitude := gsub(",", ".", longitude)]
prf_apreensoes[, latitude := gsub(",", ".", latitude)]

## Converter para numérico
prf_apreensoes[, longitude := as.numeric(longitude)]
prf_apreensoes[, latitude := as.numeric(latitude)]

## Conversão para data
prf_apreensoes[, data_apreensao := parse_date_time(data_apreensao,
                                                   orders = c("dmy", "ymd"))]

## Conversão para hora (hora_apreensao como string padrão HH:MM:SS)
prf_apreensoes[, hora_apreensao := hms::as_hms(hora_apreensao)]

## Excluindo colunas desnecessárias para as analises
prf_apreensoes[, (c("data_fato", "data_ocorrencia", "quantidade_pessoas_detidas")) := NULL]

## Removendo duplicatas com base em id de apreensão, horário e data
prf_apreensoes <- prf_apreensoes[!duplicated(prf_apreensoes[, .(id_apre, municipio_ocorrencia, data_apreensao)])]

write.csv(prf_apreensoes, "data/prf_apreensoes.csv")
