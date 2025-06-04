library(data.table)

# import data ------------------------------------------------------------------

data <- fread("data/prf_apreensoes.csv")

## Filtragens para as analises apenas do estado presente e 
data <- data[uf == "ES"]

data[, (c("uf", "estado")) := NULL]

estado_presente <- c("ARACRUZ", "CACHOEIRO DE ITAPEMIRIM", "CARIACICA", "COLATINA",
                     "LINHARES", "SÃO MATEUS", "SERRA", "VILA VELHA", "VITORIA")

data <- data[municipio_ocorrencia %in% estado_presente]
